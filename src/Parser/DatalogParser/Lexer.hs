module Parser.DatalogParser.Lexer
  ( lexeme, symbol, sc
  , variable, identifier, str
  , attributeIdentifier
  , signedInteger
  , signedFloat
  , comma, period, impliedBy
  , parens, brackets
  , domainType, dataType
  , typing
  ) where

import Relude hiding (many)

import Text.Megaparsec as P
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as C

import Control.Lens

import Language.Privalog.Types
import qualified Annotation as A
import ErrorMsg

type Parser = Parsec CompilerException Text

lexeme :: Parser a -> Parser a
lexeme = C.lexeme sc

symbol :: Text -> Parser Text
symbol = C.symbol sc

sc :: Parser ()
sc = C.space
  space1
  (C.skipLineComment "%")
  empty

variable :: Parser Text
variable = toText <$> lexeme variable' <?> "variable"
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

str :: Parser Text
str = asum
  [ lexeme $ (try sQuote) *> str' <* sQuote
  , lexeme $ (try dQuote) *> str' <* dQuote
  ]
  <?> "string"

identifier :: Parser Text
identifier = asum
  [ lexeme $ (try sQuote) *> identifier' <* sQuote
  , lexeme $ (try dQuote) *> identifier' <* dQuote
  , lexeme identifier'
  ] >>= check
  <?> "identifier"
  where 
    check :: Text -> Parser Text
    check x = 
      if x `elem` keywords
        then fail . toString $ "reserved keyword " <> x <> " cannot be an identifier."
        else return x
    keywords = ["sqrt", "is", "mod", "query"]

attributeIdentifier :: Parser Text
attributeIdentifier =
  do
    try . void $ char '@'
    lexeme identifier 
  <?> "attribute"

sQuote :: Parser Text
sQuote = symbol "\'"

dQuote :: Parser Text
dQuote = symbol "\""

identifier' :: Parser Text
identifier' = 
  do
    h <- toText . (:[]) <$> lowerChar
    t <- toText <$> (many $ alphaNumChar <|> identifierSymbols)
    return $ h <> t

identifierSymbols :: Parser Char
identifierSymbols = oneOf ['_']

str' :: Parser Text
str' =
  do
    s <- toText <$> many (anySingleBut '\'')
    return $ s

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
  <|> (symbol "int"    *> return PPInt32)
  <|> (symbol "float"  *> return PPFloat32)
  <|> (symbol "string" *> return PPStr)
  <|> (symbol "bool" *> return PPBool)
  <|> (symbol "int8" *> return PPInt8)
  <|> (symbol "int16" *> return PPInt16)
  <|> (symbol "int32" *> return PPInt32)
  <|> (symbol "int64" *> return PPInt64)
  <|> (symbol "uint8" *> return PPUInt8)
  <|> (symbol "uint16" *> return PPUInt16)
  <|> (symbol "uint32" *> return PPUInt32)
  <|> (symbol "uint64" *> return PPUInt64)
  <|> (symbol "xor_uint8" *> return PPXorUInt8)
  <|> (symbol "xor_uint16" *> return PPXorUInt16)
  <|> (symbol "xor_uint32" *> return PPXorUInt32)
  <|> (symbol "xor_uint64" *> return PPXorUInt64)
  <|> (symbol "string" *> return PPStr)
  <|> (symbol "float32" *> return PPFloat32)
  <|> (symbol "float64" *> return PPFloat64)
  <|> (symbol "auto" *> return PPAuto)
  <?> "data type"

typing :: Parser A.Ann
typing =
  do
    try . void $ symbol ":" >> notFollowedBy (char '-')
    isPK <- isJust <$> (optional $ symbol "primary")
    dom <- option Unknown domainType
    dat <- dataType
    return $ A.empty & A.annType .~ dat
                     & A.domain  .~ dom
                     & A.isPK    .~ isPK
  <?> "typing"

