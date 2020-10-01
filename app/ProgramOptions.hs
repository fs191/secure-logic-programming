{-# LANGUAGE TemplateHaskell #-}

module ProgramOptions
  ( ProgramOptions(..)
  ) where

import Control.Lens

data ProgramOptions = ProgramOptions
  { _iterations     :: Int
  , _dbCreateTables :: Bool
  , _verbose        :: Bool
  , _inFile         :: String
  , _outFile        :: String
  , _inferTypesOnly :: Bool
  }
makeLenses ''ProgramOptions

