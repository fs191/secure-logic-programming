{-# LANGUAGE FlexibleContexts #-}

module Translator 
  ( TranslatorConfig(..)
  , process
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
import Translator.Simplify
import Translator.PubPriv

import DatalogProgram
import Rule
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
    printDebug debug "Original" dp
    
    let skipSem = _skipSem conf
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

    let pubPriv = pubPrivTransform ti
    printDebug debug "PubPriv Reorder" pubPriv

    let ad2 = pubPriv & dpRules . traversed %~ adornRule
    printDebug debug "AdornRules2" ad2

    let cleared = clearTypings ad2
    printDebug debug "ClearTypings" cleared

    let ti2 = typeInference cleared
    printDebug debug "TypeInference2" ti2

    let dist = ti2 & dpRules . traversed . ruleTail %~ distribute
    printDebug debug "Distribute" dist

    post2 <- withExceptT return $ postProcess dist
    printDebug debug "PostProcess2" post2

    let ad3 = post2 & dpRules . traversed %~ adornRule
    printDebug debug "AdornRules2" ad3

    let ti3 = typeInference ad3
    printDebug debug "TypeInference3" ti3

    return ti3

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

