{-# LANGUAGE TemplateHaskell #-}

module ProgramOptions
  ( ProgramOptions(..)
  ) where

import Control.Lens

import System.Log.Logger

data ProgramOptions = ProgramOptions
  { _iterations     :: Int
  , _dbCreateTables :: Bool
  , _verbose        :: Bool
  , _inFile         :: String
  , _outFile        :: String
  }
makeLenses ''ProgramOptions

