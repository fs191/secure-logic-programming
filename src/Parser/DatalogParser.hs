{-# LANGUAGE ScopedTypeVariables #-}
module Parser.DatalogParser
  ( parseDatalogFromFile
  , parseDatalog
  ) where

import Text.Megaparsec

import Data.Void (Void)

import Control.Exception (throw)
import Control.Monad (void)
import Control.Lens

import Parser.DatalogParser.Lexer
import Parser.DatalogParser.Expr

import qualified DatalogProgram as DP
import Expr as E hiding (identifier)
import qualified Rule as R
import Annotation as A
import ErrorMsg

type Parser = Parsec Void String

data Clause
  = RC R.Rule
  | GC Expr
  | DC DP.Directive

datalogParser :: Parser DP.DatalogProgram
datalogParser = 
  do
    void sc
    st  <- manyTill clause eof
    let rs   = [x | RC x <- st]
    let dirs = [x | DC x <- st]
    let qs   = [x | GC x <- st]
    case qs of
      []  -> throw NoGoal
      [q] -> return $ DP.ppDatalogProgram rs q dirs
      x   -> throw $ TooManyGoals x

clause :: Parser Clause
clause =  label "clause" $
  do
    choice
      [ DC <$> funCall
      , RC <$> ruleP
      , GC <$> goal
      ] <* symbol "."

ruleP :: Parser R.Rule
ruleP = label "rule" $
  do
    (Pred a n xs) <- predParse
    b <- option [] $ impliedBy *> body
    let expr = joinExprs b
        t = a ^. A.typing
    (return $ R.rule n xs expr & R.ruleHead . annotation . A.typing .~ t) 

funCall :: Parser DP.Directive
funCall = 
  do
    try $ void impliedBy
    choice
      [ inputDir
      , outputDir
      , dbDir
      ]

inputDir :: Parser DP.Directive
inputDir = 
  do
    try . void $ symbol "inputs"
    _ins <- parens . brackets $ do
      sepBy attributeParse comma
    return . DP.inputDirective $ _ins

outputDir :: Parser DP.Directive
outputDir = 
  do
    try . void $ symbol "outputs"
    _ins <- parens . brackets $ do
      sepBy varParse comma
    return $ DP.outputDirective _ins

dbDir :: Parser DP.Directive
dbDir = 
  do
    try . void $ symbol "type"
    _dir <- parens $ do
      _id <- identifier
      void comma
      _ins <- brackets $ do
        sepBy attributeParse comma
      return $ DP.dbDirective _id _ins
    return _dir

body :: Parser [Expr]
body = sepBy1 aExpr comma

goal :: Parser Expr
goal = 
  do
    try . void $ symbol "?-"
    p <- predParse
    return p

-----------------------
-- Exports
-----------------------

-- | Parses a privacy datalog program from a string
parseDatalog :: String -> String -> Either (ParseErrorBundle String Void) DP.DatalogProgram
parseDatalog = runParser datalogParser

-- | Parses a privacy datalog program from a source file
parseDatalogFromFile :: String -> IO DP.DatalogProgram
parseDatalogFromFile filepath =
  do
    file <- readFile filepath
    let res = parse datalogParser filepath file
    case res of
      Left x  -> throw $ MegaparsecError x
      Right x -> return x

-----------------------
-- Utils
-----------------------

joinExprs :: [Expr] -> Expr
joinExprs [] = constTrue
joinExprs b  = foldl1 eAnd b

