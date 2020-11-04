{-# LANGUAGE TemplateHaskell #-}

module Translator 
  ( TranslatorConfig(..)
  , process
  , tcIterations
  ) where

import Relude

import Control.Lens

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
  }
makeLenses ''TranslatorConfig

process 
  :: TranslatorConfig 
  -> DatalogProgram 
  -> IO (Either CompilerException DatalogProgram)
process conf dp = runExceptT $
  do
    let ap    = adornProgram dp
    pp <- preProcess ap
    --let mag  = magicSets ap
    let ite   = _tcIterations conf
    tf <- liftIO $ deriveAllGroundRules ite pp
    let pk    = pkTransform tf
    let post' = postProcess pk

    -- currently, simplifyRule may break some annotation, so we need to derive it again
    let post = adornProgram post'

    return $ typeInference post

