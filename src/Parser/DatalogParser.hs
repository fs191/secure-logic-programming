module Parser.DatalogParser
  ( parseDatalogFromFile
  , parseDatalog
  ) where

import Text.Megaparsec

import Data.Void (Void)

import Control.Monad (void)

import Parser.DatalogParser.Lexer
import Parser.DatalogParser.Expr

import qualified DatalogProgram as DP
import Expr
import qualified Rule as R
import DBClause hiding (dataType)

type Parser = Parsec Void String

data Clause
  = RuleClause R.Rule
  | DBClause DP.DBClause

-- Based on:
-- https://www.ccs.neu.edu/home/ramsdell/tools/datalog/datalog.html#index-comment-syntax-13

datalogParser :: Parser DP.DatalogProgram
datalogParser =
  do
    -- TODO Should we parse retractions as well?
    st <- many $ try clause  <* period
    q  <- optional $ goal <* period
    eof
    let rs   = [x | RuleClause x <- st]
    let dbcs = [x | DBClause   x <- st]
    return $ DP.ppDatalogProgram rs q dbcs

clause :: Parser Clause
clause = 
      (RuleClause <$> ruleP) 
  <|> (DBClause <$> dbFact)

ruleP :: Parser R.Rule
ruleP = 
  do
    h <- identifier
    ps <- parens $ sepBy1 term comma
    b <- option [] $ impliedBy *> body
    let expr = joinExprs b
    return $ R.rule h ps expr

body :: Parser [Expr DBVar]
body = 
  sepBy1 bPredExpr comma

goal :: Parser DP.Goal
goal = 
  do
    void $ symbol "goal"
    void $ symbol "("
    i <- list
    void $ symbol ","
    o <- list
    void $ symbol ")"
    void impliedBy
    b <- body
    return $ DP.makeGoal i o $ joinExprs b

dbFact :: Parser DP.DBClause
dbFact =
  do
    impliedBy
    void $ symbol "type"
    void $ symbol "("
    n  <- identifier
    void $ symbol "("
    vs <- sepBy1 dbVar comma
    void $ symbol ")"
    void $ symbol ")"
    return $ dbClause n vs

dbVar :: Parser DBVar
dbVar =
  do
    n <- identifier
    void $ symbol ":"
    al <- domainType
    ty <- dataType
    return $ bound al ty n

list :: Parser [Expr DBVar]
list = 
  do
    void $ symbol "["
    l <- sepBy variable comma
    void $ symbol "]"
    return $ Var . free <$> l

-----------------------
-- Exports
-----------------------

parseDatalog :: String -> String -> Either (ParseErrorBundle String Void) DP.DatalogProgram
parseDatalog = runParser datalogParser

parseDatalogFromFile :: String -> IO DP.DatalogProgram
parseDatalogFromFile filepath =
  do
    file <- readFile filepath
    let res = parse datalogParser filepath file
    -- TODO use exception instead
    return $ either (error . errorBundlePretty) id res

-----------------------
-- Utils
-----------------------

joinExprs :: [Expr a] -> Expr a
joinExprs [] = ConstBool True
joinExprs b  = foldl1 (Binary And) b

