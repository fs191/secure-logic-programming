module Parser.DatalogParser
  ( parseDatalogFromFile
  , parseDatalog
  ) where

import Text.Megaparsec

import Data.Foldable
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
clause = asum
  [ try $ RC <$> ruleP
  , try $ GC <$> goal
  , DC <$> funCall
  ]

ruleP :: Parser R.Rule
ruleP = 
  do
    (Pred a n xs) <- predParse
    b <- option [] $ impliedBy *> body
    void $ symbol "."
    let expr = joinExprs b
        t = a ^. A.typing
    (return $ R.rule n xs expr & R.ruleHead . annotation . A.typing .~ t) 
      <?> "rule"

funCall :: Parser DP.Directive
funCall = asum
  [ try inputDir
  , try outputDir
  , dbDir
  ]

inputDir :: Parser DP.Directive
inputDir = 
  do
    void impliedBy
    void $ symbol "inputs"
    _ins <- parens . brackets $ do
      sepBy attributeParse comma
    void $ symbol "."
    return . DP.inputDirective $ _ins

outputDir :: Parser DP.Directive
outputDir = 
  do
    void impliedBy
    void $ symbol "outputs"
    _ins <- parens . brackets $ do
      sepBy varParse comma
    void $ symbol "."
    return $ DP.outputDirective _ins

dbDir :: Parser DP.Directive
dbDir = 
  do
    void impliedBy
    void $ symbol "type"
    _dir <- parens $ do
      _id <- identifier
      void comma
      _ins <- brackets $ do
        sepBy attributeParse comma
      return $ DP.dbDirective _id _ins
    void $ symbol "."
    return _dir

body :: Parser [Expr]
body = sepBy1 aExpr comma

goal :: Parser Expr
goal = 
  do
    p <- predParse
    void $ symbol "?"
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
    -- TODO use exception instead
    return $ either (error . errorBundlePretty) id res

-----------------------
-- Utils
-----------------------

joinExprs :: [Expr] -> Expr
joinExprs [] = constTrue
joinExprs b  = foldl1 eAnd b

