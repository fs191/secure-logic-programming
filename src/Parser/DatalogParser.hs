module Parser.DatalogParser
  ( parseDatalogFromFile
  , parseDatalog
  ) where

import Text.Megaparsec

import Data.Void (Void)

import Parser.DatalogParser.Lexer
import Parser.DatalogParser.Expr

import qualified DatalogProgram as DP
import Aexpr

type Parser = Parsec Void String

type Atom = String

type Literal = BExpr Atom

data Clause = Clause Literal [Literal]

-- Based on:
-- https://www.ccs.neu.edu/home/ramsdell/tools/datalog/datalog.html#index-comment-syntax-13

datalogParser :: Parser DP.PPDatalogProgram
datalogParser =
  do
    -- TODO Should we parse retractions as well?
    st <- many $ clause <* period
    q  <- optional query
    undefined st q

literal :: Parser Literal
literal = 
  do
    psym <- predicateSymbol
    terms <- option [] $ parens $ sepBy1 term comma
    return $ bPred psym terms

clause :: Parser Clause
clause =
  do
    h <- literal
    b <- option [] $ impliedBy *> body
    return $ Clause h b

body :: Parser [Literal]
body = sepBy1 (literal <|> bPredExpr) comma

query :: Parser Literal
query = literal <* symbol "?"


-----------------------
-- Exports
-----------------------

parseDatalog :: String -> String -> Either (ParseErrorBundle String Void) DP.PPDatalogProgram
parseDatalog = runParser datalogParser

parseDatalogFromFile :: String -> IO DP.PPDatalogProgram
parseDatalogFromFile filepath =
  do
    file <- readFile filepath
    let res = parse datalogParser filepath file
    -- TODO use exception instead
    return $ either (error . errorBundlePretty) id res

