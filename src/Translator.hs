{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Translator
  ( TranslatorOptions
  , Translator
  , withIterations
  , withBoolOnly
  , translate
  , evalTranslator
  , process
  ) where

import Control.Monad.Reader
import Control.Applicative

import Optics.TH
import Optics

import Debug.Trace

import Preprocess
import DatalogProgram
import Transform
import Optimize
import SecreC

data TranslatorOptions = TranslatorOptions
  { _iterations :: Int
  , _boolOnly   :: Bool
  }
makeLenses ''TranslatorOptions

newtype Translator a = Translator (Reader TranslatorOptions a)
  deriving
      ( Functor
      , Applicative
      , Monad
      , MonadReader TranslatorOptions
      )

defaultOptions :: TranslatorOptions
defaultOptions = TranslatorOptions
  { _iterations = 10
  , _boolOnly   = False
  }

withIterations :: Int -> Translator a -> Translator a
withIterations n = local (& iterations .~ n)

withBoolOnly :: Bool -> Translator a -> Translator a
withBoolOnly b = local (& boolOnly .~ b)

evalTranslator :: Translator a -> a
evalTranslator (Translator r) = runReader r defaultOptions

process :: PPDatalogProgram -> Translator PPDatalogProgram
process program =
  do
    cfg <- ask
    let iter = _iterations cfg
    -- apply Magic Sets or some alternative preprocessing here
    let program' = preprocess program

    -- using rules, generate all facts that we get in up to 'n' steps of rule application
    -- since we are working with symbolic data, our "facts" are actually "ground rules", i.e. rules without free variables,
    -- but they may have bounded variables that will be taken from the datbase
    -- the output is if type 'M.Map PName PMap' where:
    -- - PName is the predicate name, e.g. 'buys'
    -- - PMap maps 'arguments of the predicate' to 'RHS of the corresponding ground rule'
    let groundRules = deriveAllGroundRules program' iter

    -- we do all optimizations of computation (like constant propagation) here
    -- show the transformation result
    return $ optimize groundRules

translate :: PPDatalogProgram -> Translator String
translate program =
  do
    cfg <- ask
    let bool = _boolOnly cfg
    return $ generateSecreCscript bool program

