{-# LANGUAGE FlexibleContexts #-}

module Translator 
  ( TranslatorConfig(..)
  , process
  ) where

import Relude

import Control.Lens
import Control.Monad.Except

import Data.Text.Prettyprint.Doc

import Parser.DatalogParser
import Translator.Transform
import Translator.SemanticsChecker
import Translator.PreProcessing
import Translator.PostProcessing
import Translator.TypeInference
import Translator.Adornment
import Translator.PKTransform
import DatalogProgram
import ErrorMsg

data TranslatorConfig = TranslatorConfig
  { _tcIterations :: Int
  , _debug        :: Bool
  , _skipSem      :: Bool
  }

process 
  :: TranslatorConfig 
  -> FilePath 
  -> ExceptT [CompilerException] IO DatalogProgram
process conf path = 
  do
    src <- readFileText path
    dp <- case parseDatalog (toText path) src of
      Right x -> return x
      Left ex -> do
        putTextLn . show $ errorMsg "" ex 
        exitFailure
    let debug = _debug conf
    let skipSem = _skipSem conf
    printDebug debug "Original" dp
    --printDebug (debug && not skipSem) "SemTyped" . typeInference $ adornProgram dp
    unless skipSem $ do
      let (ex, success) = checkSemantics dp
      forM_ ex $ putTextLn . show . errorMsg src
      unless success exitFailure
    liftIO . when debug $ putTextLn "Semantics check succeeded"
    let adp = adornProgram dp
    printDebug debug "Adornment" adp
    pp <- withExceptT return $ preProcess adp
    printDebug debug "PreProcessing" pp
    --let mag  = magicSets ap
    let ite   = _tcIterations conf
    tf <- liftIO $ deriveAllGroundRules ite pp
    printDebug debug "Transform" tf
    let pk    = pkTransform tf
    printDebug debug "PKTransform" pk
    post' <- withExceptT return $ postProcess pk
    printDebug debug "PostProcess" post'
    when (null $ post' ^. dpRules) $ throwError [DoesNotConverge]
    -- currently, simplifyRule may break some annotation, so we need to derive it again
    let ad = post' & dpRules . traversed %~ adornRule
    printDebug debug "AdornRules" ad
    let ti = typeInference ad
    printDebug debug "TypeInference" ti
    return ti

printDebug 
  :: (Pretty a, MonadIO m) 
  => Bool 
  -> Text 
  -> a 
  -> m ()
printDebug debug component prog = 
    liftIO . when debug $ do
      putTextLn $ "[" <> component <> "]"
      putTextLn "=========="
      putTextLn . show $ pretty prog
      putTextLn "=========="
      putTextLn ""

