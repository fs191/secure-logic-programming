module Parser where

---------------------------------------------------------
---- Parser for DataLog programs (based on megaparsec)
---------------------------------------------------------

-- some Megaparsec-specific modules
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Control.Exception as Exc
import Control.Monad (void)
import Control.Monad.Combinators.Expr

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
allKeyWordList = ["data","int","string","private","public","type","goal"]

allKeyWords :: S.Set String -- set of reserved "words"
allKeyWords = S.fromList allKeyWordList

------------------------------------------------
-- Parsing a function as a complex expression --
------------------------------------------------

aExpr :: Parser Term
aExpr = makeExprParser aTerm aOperators

aString = do
  t <- text
  return $ AConstStr ("\'" ++ t ++ "\'")

aOperators :: [[Operator Parser Term]]
aOperators =
  [ [ InfixL (ABinary AMax <$ symbol "\\/")
    , InfixL (ABinary AMin  <$ symbol "/\\") ]

  , [ InfixL (ABinary AMult <$ symbol "*")
    , InfixL (ABinary ADiv  <$ symbol "/") ]

  , [ InfixL (ABinary AAdd <$ symbol "+")
    , InfixL (ABinary ASub <$ symbol "-") ]

  ]

aTerm :: Parser Term
aTerm = parens aExpr
  <|> AVar <$> var
  <|> AConstNum <$> signedInt
  <|> aString

bExpr :: Parser Formula
bExpr = makeExprParser bTerm bOperators

notBExpr :: Parser (Formula -> Formula)
notBExpr = do
  caseInsensKeyWord "\\+"
  return $ BUnary BNot

eqOpSymbol1 = do
  symbol "=<"
  return BLT
eqOpSymbol2 = do
  symbol "<"
  return BLE
eqOpSymbol3 = do
  symbol "=="
  return BEQ
eqOpSymbol4 = do
  symbol "="
  return BEQ
eqOpSymbol5 = do
  symbol ">="
  return BGE
eqOpSymbol6 = do
  symbol ">"
  return BGT
eqOpSymbol7 = do
  symbol "is"
  return BEQ

acompExpr :: Parser Formula
acompExpr = do
  x1 <- aExpr
  f <- eqOpSymbol1 <|> eqOpSymbol2 <|> eqOpSymbol3 <|> eqOpSymbol4 <|> eqOpSymbol5 <|> eqOpSymbol6 <|> eqOpSymbol7
  x2 <- aExpr
  return $ BBinPred f x1 x2

bpredExpr :: Parser Formula
bpredExpr = do
  (pname,args) <- ruleHead
  return $ BListPred (BPredName pname) args

bOperators :: [[Operator Parser Formula]]
bOperators =
  [
    [ Prefix notBExpr]

  --TODO we will need to use LP notation here
  , [ InfixL (BBinary BAnd <$ caseInsensKeyWord ",")]
  , [ InfixL (BBinary BOr  <$ caseInsensKeyWord ";")]

  ]

bTerm :: Parser Formula
bTerm = parens bExpr
  <|> try acompExpr
  <|> bpredExpr

------------------------------------------------------------
---- Parsing DataLog program
------------------------------------------------------------

--                        facts(database)   rules               goal: inputs, outputs, formulae to satisfy
datalogProgram :: Parser (M.Map PName PMap, M.Map PName [Rule], ([Term],[Term],[Formula]))
datalogProgram = try datalogProgramWithGoal <|> datalogProgramWithoutGoal

datalogProgramWithGoal :: Parser (M.Map PName PMap, M.Map PName [Rule], ([Term],[Term],[Formula]))
datalogProgramWithGoal = do
    (database,rules) <- manyRules <|> oneRule
    goal <- datalogGoal
    return $ (database,rules,goal)

datalogProgramWithoutGoal :: Parser (M.Map PName PMap, M.Map PName [Rule], ([Term],[Term],[Formula]))
datalogProgramWithoutGoal = do
    (database,rules) <- manyRules <|> oneRule
    return $ (database,rules,([],[],[]))

datalogGoal :: Parser ([Term],[Term],[Formula])
datalogGoal = do
    keyWord "goal"
    symbol "("
    xs <- list
    symbol ","
    ys <- list
    symbol ")"
    symbol ":-"
    formula <- sepBy1 ruleBlock (symbol ",")
    symbol "."
    return (xs,ys,formula)

list :: Parser [Term]
list = do
    symbol "["
    xs <- sepBy aTerm (symbol ",")
    symbol "]"
    return xs

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
    rhs <- ruleTail
    return $ Right (pname, [Rule args rhs])

factClause pname args = do
  void(delimRules)
  return $ Left (pname, M.singleton args (BConstBool True))

dbClause = do
  symbol ":-"
  keyWord "type"
  symbol "("
  (pname,args) <- ruleHead
  symbol ")"
  void(delimRules)
  return $ Left (pname, M.singleton args (BListPred (BPredName pname) args))

ruleHead :: Parser (PName, [Term])
ruleHead = do
  pname  <- varName
  symbol "("
  args <- sepBy aTerm (symbol ",")
  symbol ")"
  return (pname, args)

ruleTail :: Parser Formula
ruleTail = do
  --bs <- sepBy1 ruleBlock delimRows
  --void(delimRules)
  --return $ BNary BAnds bs
  bexpr <- bExpr
  void(delimRules)
  return $ bexpr

-- a rule may contain a reference to a database fact, another rule, or a boolean expression
ruleBlock :: Parser Formula
ruleBlock = try ruleBlockPred <|> ruleBlockBexpr

ruleBlockPred :: Parser Formula
ruleBlockPred = do
  (pname,args) <- ruleHead
  return $ BListPred (BPredName pname) args

ruleBlockBexpr :: Parser Formula
ruleBlockBexpr = do
  bexpr <- bExpr
  return $ bexpr

var :: Parser Var
var = try dbVar <|> freeVar

freeVar = do
    x <- varName
    return $ Free x

dbVar = do
  vName <- varName
  symbol ":"
  pType <- privacyType
  vType <- numType <|> strType
  return $ Bound pType vType vName

numType = do
    keyWord "int"
    return VarNum

strType = do
    keyWord "string"
    return VarText

privacyType :: Parser DomainType
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
        Left  x -> error $ err (errorBundlePretty x)
        Right x -> x

parseFromFile :: (Parser a) -> (String -> String -> String) -> String -> IO a
parseFromFile p err s = fmap (parseData p (err s)) (readInput s)
parseDatalogFromFile fileName = parseFromFile datalogProgram error_parseProgram fileName


