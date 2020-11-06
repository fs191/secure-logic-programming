{-# LANGUAGE TemplateHaskell #-}

module ProgramOptions
  ( ProgramOptions(..)
  ) where

import Relude

import Control.Lens

data ProgramOptions = ProgramOptions
  { _iterations     :: Int
  , _dbCreateTables :: Bool
  , _verbose        :: Bool
  , _inFile         :: Text
  , _outFile        :: Text
  , _inferTypesOnly :: Bool
  }
makeLenses ''ProgramOptions

