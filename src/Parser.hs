module Parser
  ( parseDatalogFromFile
  , parseDatalog
  ) where

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
import Data.Maybe (fromMaybe)
import Data.String
import Debug.Trace

import qualified Data.Map as M
import qualified Data.Set as S
import Rule (DomainType(..), Var, DataType(..), free, bound)
import Aexpr
import ErrorMsg
import DatalogProgram

-- Define the parser type
-- 'Void' means 'no custom error messages'
-- 'String' means 'input comes in form of a String'
type Parser = Parsec Void String

type Term = AExpr Var
type Formula = BExpr Var

data Clause
  = RuleClause Term Term
  | FactClause Term
  | DBClause

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
  <|> var
  <|> AConstNum <$> signedInt
  <|> aString

bExpr :: Parser Formula
bExpr = makeExprParser bTerm bOperators

notBExpr :: Parser (Formula -> Formula)
notBExpr = do
  caseInsensKeyWord "\\+"
  return $ BUnary BNot

eqOpSymbol :: Parser BBinPredOp
eqOpSymbol = (symbol "<=" >> return BLE)
         <|> (symbol "<"  >> return BLT)
         <|> (symbol ">=" >> return BGE)
         <|> (symbol ">"  >> return BGT)
         <|> (symbol "==" >> return BEQ)

acompExpr :: Parser Formula
acompExpr = do
  x1 <- aExpr
  x2 <- aExpr
  f <- eqOpSymbol
  return $ BBinPred f x1 x2

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

------------------------------------------------------------
---- Parsing DataLog program
------------------------------------------------------------

--                        facts(database)   rules               goal: inputs, outputs, formulae to satisfy
datalogParser :: Parser PPDatalogProgram
datalogParser = do
    clauses <- some clause
    goal <- optional datalogGoal
    undefined

datalogGoal :: Parser Goal
datalogGoal = do
    keyWord "goal"
    symbol "("
    xs <- list
    symbol ","
    ys <- list
    symbol ")"
    impliedBy
    formula <- sepBy1 ruleBlock (symbol ",")
    symbol "."
    undefined

list :: Parser [Term]
list = do
    symbol "["
    xs <- sepBy aTerm (symbol ",")
    symbol "]"
    return xs

clause :: Parser Clause
clause =
      dbClause
  <|> factClause
  <|> ruleClause

ruleClause :: Parser Clause
ruleClause = do
  rHead <- ruleHead
  rTail <- ruleTail
  undefined

factClause :: Parser Clause
factClause = do
  void(delimRules)
  undefined

dbClause :: Parser Clause
dbClause = do
  impliedBy
  keyWord "type"
  symbol "("
  rh <- ruleHead
  symbol ")"
  void(delimRules)
  undefined

ruleHead :: Parser Term
ruleHead = do
  pname  <- varName
  symbol "("
  args <- sepBy aTerm (symbol ",")
  symbol ")"
  undefined

ruleTail :: Parser Term
ruleTail = do
  --bs <- sepBy1 ruleBlock delimRows
  --void(delimRules)
  --return $ BNary BAnds bs
  bexpr <- bExpr
  void(delimRules)
  undefined

-- a rule may contain a reference to a database fact, another rule, or a boolean expression
ruleBlock :: Parser Formula
ruleBlock = try ruleBlockPred <|> ruleBlockBexpr

ruleBlockPred :: Parser Formula
ruleBlockPred = do
  rh <- ruleHead
  undefined

ruleBlockBexpr :: Parser Formula
ruleBlockBexpr = do
  bexpr <- bExpr
  return $ bexpr

var :: Parser (AExpr Var)
var = AVar <$> (try dbVar <|> freeVar)

freeVar :: Parser Var
freeVar = do
    x <- varName
    return $ free x

dbVar :: Parser Var
dbVar = do
  vName <- varName
  symbol ":"
  pType <- privacyType
  vType <-
        (keyWord "int"    >> return VarNum)
    <|> (keyWord "string" >> return VarText)
  return $ bound pType vType vName

privacyType :: Parser DomainType
privacyType =
      (keyWord "private" >> return Private)
  <|> (keyWord "public"  >> return Public)

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
varName :: Parser String
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

parseDatalog :: String -> String -> Either (ParseErrorBundle String Void) PPDatalogProgram
parseDatalog filepath source = runParser datalogParser filepath source

parseDatalogFromFile :: String -> IO PPDatalogProgram
parseDatalogFromFile filepath =
  do
    file <- readFile filepath
    let res = parse datalogParser filepath file
    -- TODO use exception instead
    return $ either (error . errorBundlePretty) id res

