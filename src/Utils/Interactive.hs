{-# LANGUAGE FlexibleContexts #-}
module Utils.Interactive
  ( interactiveQueries
  , process
  ) where

import Relude

import Control.Lens
import Control.Monad.Except
import Data.Generics.Uniplate.Data as U

import Data.Text.Prettyprint.Doc

import Parser.DatalogParser

import ErrorMsg
import Annotation
import DatalogProgram
import Rule
import Expr
import ProgramOptions

import Translator.EnumQueries

process 
  :: ( MonadIO m
     , MonadReader ProgramOptions m
     , MonadError CompilerException m
     )
  => m DatalogProgram
process = 
  do
    path <- view inFile
    src <- readFileText $ toString path
    dp <- case parseDatalog (toText path) src of
      Right x -> return x
      Left ex -> do
        putTextLn . show $ errorMsg "" ex 
        exitFailure
    dbg <- view debug
    printDebug "Original" dp

    eqp <- enumerateQueries dp
    printDebug "EnumQueries" eqp

    return eqp

printDebug 
  :: ( Pretty a
     , MonadIO m
     , MonadReader ProgramOptions m
     )
  => Text 
  -> a 
  -> m ()
printDebug component prog = 
  do
    dbg <- view debug
    liftIO . when dbg $ do
      putTextLn $ "[" <> component <> "]"
      putTextLn "=========="
      putTextLn . show $ pretty prog
      putTextLn "=========="
      putTextLn ""

interactiveQueries :: DatalogProgram -> [Text]
interactiveQueries dp =
    let xs = concat $ map ruleQueryExprs (dp ^. dpRules) in
    let ys = sortBy (\(x1,_) (x2,_) -> compare x1 x2) xs in
    map snd ys

ruleQueryExprs :: Rule -> [(Int,Text)]
ruleQueryExprs r = queryExprs (r ^. ruleTail)

