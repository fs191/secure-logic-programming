{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Translator
  ( TranslatorOptions
  , Translator
  , withIterations
  ) where

import Control.Monad.Reader
import Control.Applicative

import Optics.TH
import Optics

data TranslatorOptions = TranslatorOptions
  { _iterations :: Int
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
  }

withIterations :: Translator a -> Int -> Translator a
withIterations t n = local (& iterations .~ n) t
