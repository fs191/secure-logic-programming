module Parser.DatalogParser.Lexer
  ( lexeme, symbol, sc
  , variable, identifier
  , attributeIdentifier
  , signedInteger
  , comma, period, impliedBy
  , parens, brackets
  , domainType, dataType
  , typing
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as C

import Data.Foldable
import Data.Void (Void)

import Control.Monad (void)

import Language.SecreC.Types

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
variable = lexeme $
  do
    h <- upperChar
    t <- many $ alphaNumChar <|> identifierSymbols
    return $ h:t

identifier :: Parser String
identifier = asum
  [ try $ between sQuote sQuote identifier'
  , try $ between dQuote dQuote identifier'
  , identifier'
  ] <?> "identifier"

attributeIdentifier :: Parser String
attributeIdentifier = 
  do
    void $ char '@'
    ('@':) <$> identifier

sQuote :: Parser String
sQuote = symbol "'"

dQuote :: Parser String
dQuote = symbol "\""

identifier' :: Parser String
identifier' = lexeme $
  do
    h <- lowerChar <|> identifierSymbols
    t <- many $ alphaNumChar <|> identifierSymbols
    return $ h:t

identifierSymbols :: Parser Char
identifierSymbols = oneOf ['_']

signedInteger :: Parser Int
signedInteger = lexeme $ C.signed sc C.decimal

comma :: Parser ()
comma = void $ symbol ","

period :: Parser ()
period = void $ symbol "."

parens :: Parser a -> Parser a
parens = lexeme . between (char '(') (char ')')

brackets :: Parser a -> Parser a
brackets = lexeme . between (char '[') (char ']')

impliedBy :: Parser ()
impliedBy = void $ symbol ":-"

domainType :: Parser PPDomain
domainType =
      (try $ symbol "public"  *> return Public)
  <|> (symbol "private" *> return Private)

dataType :: Parser PPType
dataType =
      try (symbol "bool"   *> return PPBool)
  <|> try (symbol "int"    *> return PPInt)
  <|> (symbol "string" *> return PPStr)

typing :: Parser (PPDomain, PPType)
typing =
  do
    void $ symbol ":"
    dom <- domainType
    dat <- dataType
    return (dom, dat)

