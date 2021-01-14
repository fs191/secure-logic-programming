{-# LANGUAGE FlexibleContexts #-}

module Translator 
  ( process
  ) where

import Relude

import Control.Lens
import Control.Monad.Except
import qualified Data.Generics.Uniplate.Data as U

import Data.Text.Prettyprint.Doc

import Parser.DatalogParser

import Translator.Adornment
import Translator.PKTransform
import Translator.PostProcessing
import Translator.PreProcessing
import Translator.SemanticsChecker
import Translator.Transform
import Translator.TypeInference
import Translator.Distribute
import Translator.PubPriv
import Translator.CoerceTypes

import DatalogProgram
import Rule
import ErrorMsg
import ProgramOptions

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
    
    skipSem <- view skipSemCheck
    unless skipSem $ do
      let (ex, success) = checkSemantics dp
      forM_ ex $ putTextLn . show . errorMsg src
      unless success exitFailure
    liftIO . when dbg $ putTextLn "Semantics check succeeded"
    let adp = adornProgram dp
    printDebug "Adornment" adp

    pp <- preProcess adp
    printDebug "PreProcessing" pp

    --let mag  = magicSets ap
    tf <- deriveAllGroundRules pp
    printDebug "Transform" tf

    let pk    = pkTransform tf
    printDebug "PKTransform" pk

    post' <- postProcess pk
    printDebug "PostProcess" post'
    when (null $ post' ^. dpRules) $ throwError DoesNotConverge

    -- currently, simplifyRule may break some annotation, so we need to derive it again
    let ad = post' & dpRules . traversed %~ adornRule
    printDebug "AdornRules" ad

    let ti = typeInference ad
    printDebug "TypeInference" ti

    let pubPriv = pubPrivTransform ti
    printDebug "PubPriv Reorder" pubPriv

    let ad2 = pubPriv & dpRules . traversed %~ adornRule
    printDebug "AdornRules2" ad2

--    let cleared = clearTypings ad2
--    printDebug "ClearTypings" cleared

    let ti2 = typeInference ad2
    printDebug "TypeInference2" ti2

    let dist = ti2 & dpRules . traversed . ruleTail %~ distribute
    printDebug "Distribute" dist

    post2 <- postProcess dist
    printDebug "PostProcess2" post2

    let ad3 = post2 & dpRules . traversed %~ adornRule
    printDebug "AdornRules2" ad3

    let ti3 = typeInference ad3
    printDebug "TypeInference3" ti3

    let co = ti3 & dpRules . traversed . ruleTail %~ U.rewrite coerceTypes
    printDebug "TypeCoercion" co

    return co

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

