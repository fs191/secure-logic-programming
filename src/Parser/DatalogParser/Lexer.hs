{-# LANGUAGE OverloadedStrings #-}

module Parser.DatalogParser.Lexer
  ( tokenize
  ) where

import Text.Megaparsec as P
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as C

import Control.Exception
import Control.Monad

import Data.List
import Data.Void (Void)

import ErrorMsg

type Parser = Parsec Void String

data PrivalogToken
  = Integer Int
  | Float Float
  | Variable String
  | Atom String
  | String String
  | Operator String
  | BracketL
  | BracketR
  | ParenL
  | ParenR

data PrivalogTokens = PrivalogTokens [PrivalogToken]

instance Show PrivalogToken where
  show (Integer x) = show x ++ ":int"
  show (Float x) = show x ++ ":float"
  show (Variable x) = x ++ ":var"
  show (Atom x) = x ++ ":atom"
  show (String x) = x ++ ":str"
  show (Operator x) = x ++ ":op"
  show BracketL = "[:br"
  show BracketR = "]:br"
  show ParenL = "(:par"
  show ParenR = "):par"

instance Show PrivalogTokens where
  show (PrivalogTokens x) = intercalate " " $ show <$> x

parseToken :: Parser PrivalogToken
parseToken = label "token" . lexeme $ choice
  [ Integer  <$> plInteger
  , Float    <$> plFloat
  , Variable <$> variable
  , Atom     <$> atom
  , String   <$> plString
  , Operator <$> operator
  , BracketL <$  (label "left bracket" $ symbol "[")
  , BracketR <$  (label "right bracket" $ symbol "]")
  , ParenL   <$  (label "left paren" $ symbol "(")
  , ParenR   <$  (label "right paren" $ symbol ")")
  ]

plInteger :: Parser Int
plInteger = label "integer" . lexeme $ C.decimal

plFloat :: Parser Float
plFloat = label "float" . lexeme $ C.float

plString :: Parser String
plString = label "string" . lexeme $
  do
    void $ char '"'
    manyTill C.charLiteral $ char '"'

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
variable = label "variable" . lexeme $
      do
        h <- upperChar <|> char '_'
        t <- many $ alphaNumChar <|> char '_'
        return $ h:t

atom :: Parser String
atom = label "atom" . lexeme $ choice
  [ do
      h <- lowerChar
      t <- many $ alphaNumChar <|> char '_'
      return $ h:t
  , do
      void $ char '\''
      manyTill C.charLiteral $ char '\''
  ]

operator :: Parser String
operator = label "operator" . lexeme $
  some $ oneOf 
    [ '#', '$', '&', '*'
    , '+', '-', '.', '/'
    , ':', '<', '=', '>'
    , '?', '@', '^', '~'
    , ';', ',', '\\'
    ]

tokenize :: String -> PrivalogTokens
tokenize t = 
  case PrivalogTokens <$> runParser (sc *> many parseToken <* eof) "" t of
    Left e  -> throw $ LexerError e
    Right x -> x

