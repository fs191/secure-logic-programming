module Parser.DatalogParser
  ( parseDatalogFromFile
  , parseDatalog
  ) where

import Text.Megaparsec

import Data.Foldable
import Data.Void (Void)
import Data.Maybe

import Control.Monad (void)

import Parser.DatalogParser.Lexer
import Parser.DatalogParser.Expr

import qualified DatalogProgram as DP
import Expr
import qualified Rule as R

type Parser = Parsec Void String

data Clause
  = RC R.Rule
  | GC DP.Goal
  | DC DP.Directive

-- Based on:
-- https://www.ccs.neu.edu/home/ramsdell/tools/datalog/datalog.html#index-comment-syntax-13

datalogParser :: Parser DP.DatalogProgram
datalogParser =
  do
    st  <- manyTill clause eof
    let rs   = [x | RC x <- st]
    let dirs = [x | DC x <- st]
    let q    = listToMaybe [x | GC x <- st]
    return $ DP.ppDatalogProgram rs q dirs

clause :: Parser Clause
clause = asum
  [ try $ RC  <$> ruleP
  , try $ GC <$> goal
  , DC <$> funCall
  ]

ruleP :: Parser R.Rule
ruleP = 
  do
    h <- identifier
    ps <- parens $ sepBy1 term comma
    b <- option [] $ impliedBy *> body
    void $ symbol "."
    let expr = joinExprs b
    return $ R.rule h ps expr

funCall :: Parser DP.Directive
funCall =
  do 
    impliedBy
    n <- identifier
    ps <- parens $ sepBy1 aExpr comma
    void $ symbol "."
    return $ DP.directive n ps

body :: Parser [Expr]
body = sepBy1 bPredExpr comma

goal :: Parser DP.Goal
goal = 
  do
    h <- identifier
    ps <- parens $ sepBy1 term comma
    let p = predicate h ps
    void $ symbol "?"
    return $ DP.makeGoal [] [] p

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

joinExprs :: [Expr] -> Expr
joinExprs [] = constTrue
joinExprs b  = foldl1 eAnd b

