{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Translator 
  ( TranslatorConfig(..)
  , process
  ) where

import Relude

import Control.Monad.Except

import Data.Text.Prettyprint.Doc

import Transform
import PreProcessing
import PostProcessing
import TypeInference
import Adornment
import PKTransform
import DatalogProgram
import ErrorMsg

data TranslatorConfig = TranslatorConfig
  { _tcIterations :: Int
  , _debug        :: Bool
  }

process 
  :: (MonadIO m, MonadError CompilerException m)
  => TranslatorConfig 
  -> DatalogProgram 
  -> m DatalogProgram
process conf dp = 
  do
    let debug = _debug conf
    printDebug debug "Original" dp
    let adp = adornProgram dp
    printDebug debug "Adornment" adp
    pp <- preProcess adp
    printDebug debug "PreProcessing" pp
    --let mag  = magicSets ap
    let ite   = _tcIterations conf
    tf <- liftIO $ deriveAllGroundRules ite pp
    printDebug debug "Transform" tf
    let pk    = pkTransform tf
    printDebug debug "PKTransform" pk
    post' <- postProcess pk
    printDebug debug "PostProcess" post'
    -- currently, simplifyRule may break some annotation, so we need to derive it again
    let ad = adornProgram post'

    return $ typeInference ad

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

