{-# LANGUAGE TemplateHaskell #-}

module Translator 
  ( TranslatorConfig(..)
  , process
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

data TranslatorConfig = TranslatorConfig
  { _tcIterations :: Int
  }
makeLenses ''TranslatorConfig

process :: TranslatorConfig -> DatalogProgram -> IO DatalogProgram
process conf dp =
  do
    let ap   = adornProgram dp
    let pp   = preProcess ap
    --let mag  = magicSets ap
    let ite = _tcIterations conf
    tf <- deriveAllGroundRules ite pp
    let pk   = pkTransform tf
    let post' = postProcess pk

    -- currently, simplifyRule may break some annotation, so we need to derive it again
    let post = adornProgram post'

    return $ typeInference post

