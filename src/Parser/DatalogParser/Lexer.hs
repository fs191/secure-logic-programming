module Parser.DatalogParser.Lexer 
  ( lexeme, symbol
  , sc
  , variable, identifier
  , stringLiteral
  , predicateSymbol
  , signedInteger
  , comma, period, impliedBy
  , parens
  , domainType, dataType
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as C

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
identifier = lexeme $
  do
    h <- lowerChar <|> identifierSymbols
    t <- many $ alphaNumChar <|> identifierSymbols
    return $ h:t

identifierSymbols :: Parser Char
identifierSymbols = oneOf ['_', '-']

stringLiteral :: Parser String
stringLiteral = 
      (try $ strLike '\'')
  <|> (try $ strLike '\"')

strLike :: Char -> Parser String
strLike c = lexeme $ char c *> manyTill C.charLiteral (char c)

signedInteger :: Parser Int
signedInteger = lexeme $ C.signed sc C.decimal
  
comma :: Parser ()
comma = void $ symbol ","

period :: Parser ()
period = void $ symbol "."

parens :: Parser a -> Parser a
parens = lexeme . between (char '(') (char ')')

impliedBy :: Parser ()
impliedBy = void $ symbol ":-"

predicateSymbol :: Parser String
predicateSymbol = (try identifier) <|> stringLiteral

domainType :: Parser PPDomain
domainType =
      (try $ symbol "public"  *> return Public)
  <|> (try $ symbol "private" *> return Private)

dataType :: Parser PPType
dataType =
      (try $ symbol "bool"   *> return PPBool)
  <|> (try $ symbol "int"    *> return PPInt)
  <|> (try $ symbol "string" *> return PPStr)

