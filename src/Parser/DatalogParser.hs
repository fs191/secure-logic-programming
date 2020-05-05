module Parser.DatalogParser
  ( parseDatalogFromFile
  , parseDatalog
  ) where

import Text.Megaparsec
import Text.Megaparsec.Debug

import Data.Void (Void)

import Control.Monad (void)

import Parser.DatalogParser.Lexer
import Parser.DatalogParser.Expr

import qualified DatalogProgram as DP
import Aexpr
import Rule

type Parser = Parsec Void String

-- Based on:
-- https://www.ccs.neu.edu/home/ramsdell/tools/datalog/datalog.html#index-comment-syntax-13

datalogParser :: Parser DP.PPDatalogProgram
datalogParser =
  do
    -- TODO Should we parse retractions as well?
    st <- many $ try $ clause  <* period
    q  <- optional $ goal
    eof
    let dp = DP.fromRulesAndGoal st q
    return $ DP.ppDatalogProgram dp []

clause :: Parser Rule
clause = 
  do
    h <- literal
    b <- option [] $ impliedBy *> body
    let expr = joinExprs b
    return $ rule (functor h) (args h) expr

body :: Parser [BExpr DBVar]
body = dbg "body" $
  sepBy1 p comma
    where p = bPredExpr

goal :: Parser DP.Goal
goal = dbg "goal" $
  do
    void $ symbol "goal"
    void $ symbol "("
    i <- list
    void $ symbol ","
    o <- list
    void $ symbol ")"
    void impliedBy
    b <- body
    void period
    return $ DP.makeGoal i o $ joinExprs b

list :: Parser [Term]
list = dbg "list" $
  do
    void $ symbol "["
    l <- sepBy variable comma
    void $ symbol "]"
    return $ aVar <$> l

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

-----------------------
-- Utils
-----------------------

joinExprs :: [BExpr a] -> BExpr a
joinExprs [] = bTrue
joinExprs b  = foldl1 bAnd b

