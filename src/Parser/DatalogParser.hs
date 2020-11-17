{-# LANGUAGE ScopedTypeVariables #-}
module Parser.DatalogParser
  ( parseDatalogFromFile, parseDatalogFromFile_
  , parseDatalog
  ) where

import Relude

import Text.Megaparsec

import Control.Exception (throw)
import Control.Lens

import Parser.DatalogParser.Lexer
import Parser.DatalogParser.Expr

import qualified DatalogProgram as DP
import Expr as E hiding (identifier)
import qualified Rule as R
import Annotation as A
import ErrorMsg

type Parser = Parsec CompilerException Text

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
      []  -> error $ show NoGoal
      [q] -> return $ DP.ppDatalogProgram rs q dirs
      _   -> customFailure TooManyGoals

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
    return $ R.rule n xs expr 
      & R.ruleHead . annotation . A.typing .~ t
      & R.ruleHead . annotation . A.srcPos .~ (a ^. srcPos)

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
    parens $ do
      pred <- predParse
      return $ DP.dbDirective pred

body :: Parser [Expr]
body = sepBy1 aExpr comma

goal :: Parser Expr
goal = 
  do
    try . void $ symbol "?-"
    try aggregation <|> predParse

-----------------------
-- Exports
-----------------------

-- | Parses a privacy datalog program from a string
parseDatalog :: Text -> Text -> Either CompilerException DP.DatalogProgram
parseDatalog path content = 
    case res of
      Left x  -> Left . ParserException . toText $ errorBundlePretty x
      Right x -> Right x
  where
    res = runParser datalogParser (toString path) content

-- | Parses a privacy datalog program from a source file
parseDatalogFromFile :: Text -> IO (Either CompilerException DP.DatalogProgram)
parseDatalogFromFile filepath =
  do
    file <- readFileText (toString filepath)
    let res = parse datalogParser (toString filepath) file
    case res of
      Left x  -> 
          return . Left . ParserException . toText $ errorBundlePretty x
      Right x -> return $ Right x

-- | Unsafe version of `parseDatalogFromFile`
parseDatalogFromFile_ :: Text -> IO DP.DatalogProgram
parseDatalogFromFile_ f = either throw id <$> parseDatalogFromFile f

-----------------------
-- Utils
-----------------------

joinExprs :: [Expr] -> Expr
joinExprs [] = constTrue
joinExprs (h:t)  = foldl' eAnd h t

