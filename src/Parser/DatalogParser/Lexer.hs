module Parser.DatalogParser.Lexer 
  ( lexeme, symbol
  , sc
  , variable, identifier
  , stringLiteral
  , predicateSymbol
  , signedInteger
  , comma, period, impliedBy
  , parens
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as C

import Data.Void (Void)

import Control.Monad (void)

import Rule

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

variable :: Parser DBVar
variable = lexeme $
  do
    h <- upperChar
    t <- many $ alphaNumChar <|> identifierSymbols
    return . free $ h:t

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
signedInteger = C.signed sc C.decimal
  
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

