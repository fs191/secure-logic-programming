module Parser where

---------------------------------------------------------
---- Parser for DataLog programs (based on megaparsec)
---------------------------------------------------------

-- some Megaparsec-specific modules
import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Control.Exception as Exc
import Control.Monad (void)

import Data.Either
import Data.Void
import Data.Char
import Debug.Trace

import qualified Data.Map as M
import qualified Data.Set as S
import Aexpr
import ErrorMsg
import Rule

-- Define the parser type
-- 'Void' means 'no custom error messages'
-- 'String' means 'input comes in form of a String'
type Parser = Parsec Void String


---------------------------------------------------------------------------------------------
-- keywords
allKeyWordList :: [String]
allKeyWordList = ["data","int","string","private","public"]

allKeyWords :: S.Set String -- set of reserved "words"
allKeyWords = S.fromList allKeyWordList

------------------------------------------------
-- Parsing a function as a complex expression --
------------------------------------------------

aExpr :: Parser Arg
aExpr = makeExprParser aTerm aOperators

aString = do
  t <- text
  return $ AConstStr ("\'" ++ t ++ "\'")

aOperators :: [[Operator Parser Arg]]
aOperators =
  [ [ InfixL (ABinary AMax <$ symbol "\\/")
    , InfixL (ABinary AMin  <$ symbol "/\\") ]

  , [ InfixL (ABinary AMult <$ symbol "*")
    , InfixL (ABinary ADiv  <$ symbol "/") ]

  , [ InfixL (ABinary AAdd <$ symbol "+")
    , InfixL (ABinary ASub <$ symbol "-") ]

  ]

aTerm :: Parser Arg
aTerm = parens aExpr
  <|> AVar <$> var
  <|> AConstNum <$> signedInt
  <|> aString

bExpr :: Parser Arg
bExpr = makeExprParser bTerm bOperators

notAExpr :: Parser (AExpr a -> AExpr a)
notAExpr = do
  caseInsensKeyWord "\\+"
  return $ AUnary ANot

bOperators :: [[Operator Parser Arg]]
bOperators =
  [
    [ Prefix notAExpr]

  , [ InfixL (ABinary ALE <$ symbol "=<")
    , InfixL ((\x y -> AUnary ANot (ABinary AEQ x y)) <$ symbol "<>")
    , InfixL ((\x y -> AUnary ANot (ABinary AEQ x y)) <$ symbol "!=")
    , InfixL (ABinary ALT <$ symbol "<")
    , InfixL (ABinary AEQ <$ symbol "==")
    , InfixL (ABinary AEQ <$ symbol "=")
    , InfixL (ABinary AGE <$ symbol ">=")
    , InfixL (ABinary AGT <$ symbol ">") ]

  --TODO we will need to use LP notation here
  , [ InfixL (ABinary AAnd <$ caseInsensKeyWord "and")]
  , [ InfixL (ABinary AOr  <$ caseInsensKeyWord "or")]

  ]

bTerm :: Parser Arg
bTerm = aExpr <|> parens bExpr

------------------------------------------------------------
---- Parsing DataLog program
------------------------------------------------------------

-- TODO at some point, we want an aexpr instead of [RHS] everywhere, since there can be disjunctions as well
datalogProgram :: Parser (M.Map PName PMap, M.Map PName [Rule], [RHS])
datalogProgram = try datalogProgramWithGoal <|> datalogProgramWithoutGoal

datalogProgramWithGoal :: Parser (M.Map PName PMap, M.Map PName [Rule], [RHS])
datalogProgramWithGoal = do
    (database,rules) <- manyRules <|> oneRule
    goal <- datalogGoal
    return $ (database,rules,goal)

datalogProgramWithoutGoal :: Parser (M.Map PName PMap, M.Map PName [Rule], [RHS])
datalogProgramWithoutGoal = do
    (database,rules) <- manyRules <|> oneRule
    return $ (database,rules,[])

datalogGoal :: Parser [RHS]
datalogGoal = do
    symbol "?-"
    rhs <- sepBy1 ruleBlock (symbol ",")
    symbol "."
    return rhs

manyRules :: Parser (M.Map PName PMap, M.Map PName [Rule])
manyRules = do
    xs <- many clause
    let xs1 = lefts xs  --database facts
    let xs2 = rights xs --rules
    return (M.fromListWith (M.union) xs1, M.fromListWith (++) xs2)

oneRule :: Parser (M.Map PName PMap, M.Map PName [Rule])
oneRule = do
    x <- clause
    let xs1 = case x of {Left  z -> [z]; _ -> []}  --database facts
    let xs2 = case x of {Right z -> [z]; _ -> []}  --rule
    return (M.fromList xs1, M.fromList xs2)

clause :: Parser (Either (PName, PMap) (PName, [Rule]))
clause = do
    dbClause <|> ruleOrFactClause

ruleOrFactClause = do
    (pname,args) <- ruleHead
    ruleClause pname args <|> factClause pname args

ruleClause pname args = do
    void (impliedBy)
    rs <- ruleTail
    return $ Right (pname, [Rule args rs])

factClause pname args = do
  void(delimRules)
  return $ Left (pname, M.singleton args (AConstNum 1))

dbClause = do
  keyWord ("data")
  (pname,args) <- ruleHead
  void(delimRules)
  return $ Left (pname, M.singleton args (ANary (AMember pname) args))

ruleHead :: Parser (PName, [Arg])
ruleHead = do
  pname  <- varName
  symbol "("
  args <- sepBy1 aTerm (symbol ",")
  symbol ")"
  return (pname, args)

ruleTail :: Parser [RHS]
ruleTail = do
  bs <- sepBy1 ruleBlock delimRows
  void(delimRules)
  return bs

-- a rule may contain a reference to a database fact, another rule, or a boolean expression
ruleBlock :: Parser RHS
ruleBlock = try ruleBlockPred <|> ruleBlockBexpr

ruleBlockPred :: Parser RHS
ruleBlockPred = do
  (pname,args) <- ruleHead
  return $ Fact pname args

ruleBlockBexpr :: Parser RHS
ruleBlockBexpr = do
  bexpr <- bExpr
  return $ ABB bexpr

var :: Parser Var
var = dbVar <|> freeVar

freeVar = do
    x <- varName
    return $ Free x

dbVar = do
  pType <- privacyType
  vType <- numType <|> strType
  vName <- varName
  return $ pType vType vName

numType = do
    keyWord "int"
    return VarNum

strType = do
    keyWord "string"
    return VarText

privacyType :: Parser (VarType -> AName -> Var)
privacyType = privateType <|> publicType

privateType = do
  keyWord "private"
  return Private

publicType = do
  keyWord "public"
  return Public

------------------------------
---- Symbols and keywords ----
------------------------------

-- delimiter of rows
delimRows :: Parser String
delimRows = symbol ","

-- delimiter of rules
delimRules :: Parser String
delimRules = symbol "."

-- implication
impliedBy :: Parser String
impliedBy = symbol ":-"

-- line comment
lineComment :: String
lineComment = "%"

-- block comment
blockCommentStart :: String
blockCommentStart = "/*"

blockCommentEnd :: String
blockCommentEnd = "*/"

-------------------------------------
---- Some auxiliary subparsers   ----
-------------------------------------

-- a keyword
keyWord :: String -> Parser ()
keyWord w = lexeme (C.string w *> notFollowedBy C.alphaNumChar)

readKeyWord :: String -> Parser String
readKeyWord w = do
    lexeme (C.string w *> notFollowedBy C.alphaNumChar)
    return w

caseInsensKeyWord :: String -> Parser ()
caseInsensKeyWord w = lexeme (C.string' w *> notFollowedBy C.alphaNumChar)

-- variable identifier, as taken from the tutorial
-- it checks that the identifier is not a keyword
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> C.letterChar <*> many alphaNumCharAndSubscript
    check x = if S.member (map toLower x) allKeyWords
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x

alphaNumCharAndSubscript :: Parser Char
alphaNumCharAndSubscript = C.char '_'
    <|> C.alphaNumChar

-- we need to read string identifiers and afterwards map them to integers
varName :: Parser VName
varName = identifier

--reads an arbitrary string, all characters up to the first space
text :: Parser String
text = lexeme (C.char '\'' >> manyTill L.charLiteral (C.char '\''))

-- this thing eats all spaces and comments
spaceConsumer :: Parser ()
spaceConsumer = 
        L.space C.space1 lineCmnt blockCmnt
    where
        lineCmnt  = L.skipLineComment lineComment
        blockCmnt = L.skipBlockComment blockCommentStart blockCommentEnd

-- reads a lexeme and removes all trailing whitespaces and comments
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- reads a pure string and removes all trailing whitespaces and comments
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- reads an integer
integer :: Parser Int
integer = lexeme L.decimal

-- reads a double
float :: Parser Double
float = try (lexeme L.float) <|> fmap fromIntegral integer

-- reads a signed integer
signedInt :: Parser Int
signedInt = L.signed spaceConsumer integer

-- reads a signed double
signedFloat :: Parser Double
signedFloat = try (L.signed spaceConsumer float) <|> fmap fromIntegral (L.signed spaceConsumer integer)


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

--------------------------
---- Parser embedding ----
--------------------------

-- read the file line by line -- no special parsing, assume that the delimiters are whitespaces
readInput :: String -> IO String
readInput path = do
   content <- readFile path
   return content

-- this is to extract the actual parsed data
parseData :: (Parser a) -> (String -> String) -> String -> a
parseData p err s = 
    let res = parse p "" s in
    case res of
        Left  x -> error $ err (parseErrorPretty x)
        Right x -> x

parseFromFile :: (Parser a) -> (String -> String -> String) -> String -> IO a
parseFromFile p err s = fmap (parseData p (err s)) (readInput s)
parseDatalogFromFile fileName = parseFromFile datalogProgram error_parseProgram fileName


