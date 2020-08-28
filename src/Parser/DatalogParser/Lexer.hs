module Parser.DatalogParser.Lexer
  ( lexeme, symbol, sc
  , variable, identifier
  , attributeIdentifier
  , signedInteger
  , signedFloat
  , comma, period, impliedBy
  , parens, brackets
  , domainType, dataType
  , typing
  ) where

import Text.Megaparsec as P
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as C

import Data.Maybe
import Data.Foldable
import Data.Void (Void)

import Control.Lens
import Control.Monad (void)

import Language.SecreC.Types
import qualified Annotation as A

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = C.lexeme sc

symbol :: String -> Parser String
symbol = C.symbol sc

sc :: Parser ()
sc = C.space
  space1
  (C.skipLineComment "%")
  empty

variable :: Parser String
variable = lexeme variable' <?> "variable"
  where
    variable' =
      do
        h <- upperChar
        t <- many $ alphaNumChar <|> identifierSymbols
        return $ h:t
      <|>
      do
        h <- char '_'
        t <- many $ alphaNumChar <|> identifierSymbols
        return $ h:t


identifier :: Parser String
identifier = asum
  [ lexeme $ (try sQuote) *> identifier' <* sQuote
  , lexeme $ (try dQuote) *> identifier' <* dQuote
  , lexeme identifier'
  ] >>= check
  <?> "identifier"
  where 
    check :: String -> Parser String
    check x = 
      if x `elem` keywords
        then fail $ "reserved keyword " ++ x ++ " cannot be an identifier."
        else return x
    keywords = ["sqrt", "is"]

attributeIdentifier :: Parser String
attributeIdentifier =
  do
    try . void $ char '@'
    lexeme identifier 
  <?> "attribute"

sQuote :: Parser String
sQuote = symbol "'"

dQuote :: Parser String
dQuote = symbol "\""

identifier' :: Parser String
identifier' = 
  do
    h <- lowerChar
    t <- many $ alphaNumChar <|> identifierSymbols
    return $ h:t

identifierSymbols :: Parser Char
identifierSymbols = oneOf ['_']

signedInteger :: Parser Int
signedInteger = lexeme $ C.signed sc C.decimal

signedFloat :: Parser Float
signedFloat = lexeme $ C.signed sc C.float

comma :: Parser ()
comma = void $ symbol ","

period :: Parser ()
period = void $ symbol "."

parens :: Parser a -> Parser a
parens = lexeme . between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = lexeme . between (symbol "[") (symbol "]")

impliedBy :: Parser ()
impliedBy = void $ symbol ":-"

domainType :: Parser PPDomain
domainType =
      (symbol "public"  *> return Public)
  <|> (symbol "private" *> return Private)
  <?> "privacy type"

dataType :: Parser PPType
dataType =
      (symbol "bool"   *> return PPBool)
  <|> (symbol "int"    *> return PPInt)
  <|> (symbol "float"  *> return PPFloat)
  <|> (symbol "string" *> return PPStr)
  <?> "data type"

typing :: Parser A.Ann
typing =
  do
    void $ symbol ":"
    isPK <- isJust <$> (optional $ symbol "primary")
    dom <- option Unknown domainType
    dat <- dataType
    return $ A.empty & A.annType .~ dat
                     & A.domain  .~ dom
                     & A.isPK    .~ isPK
  <?> "typing"

