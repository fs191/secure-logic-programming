{-# LANGUAGE TemplateHaskell #-}

module ProgramOptions
  ( ProgramOptions(..)
  , iterations
  , dbCreateTables
  , verbose
  , inFile
  , outFile
  , inferTypesOnly
  , debug
  , skipSemCheck
  , defaultOptions
  ) where

import Relude

import Control.Lens

data ProgramOptions = ProgramOptions
  { _iterations     :: Int
  , _dbCreateTables :: Bool
  , _verbose        :: Bool
  , _inferTypesOnly :: Bool
  , _outFile        :: Text
  , _debug          :: Bool
  , _inFile         :: Text
  , _skipSemCheck   :: Bool
  }
makeLenses ''ProgramOptions

defaultOptions :: ProgramOptions
defaultOptions = ProgramOptions
  { _iterations = 10
  , _dbCreateTables = False
  , _verbose = False
  , _inferTypesOnly = False
  , _outFile = "out"
  , _debug = False
  , _inFile = ""
  , _skipSemCheck = False
  }

