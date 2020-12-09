{-# LANGUAGE TemplateHaskell #-}

module ProgramOptions
  ( ProgramOptions(..)
  , InliningStrategy(..)
  , iterations
  , dbCreateTables
  , verbose
  , inFile
  , outFile
  , inferTypesOnly
  , debug
  , skipSemCheck
  , inliningStrategy
  , defaultOptions
  ) where

import Relude

import Control.Lens

data InliningStrategy
  = BreadthFirst
  | DepthFirst
  | FullGround

data ProgramOptions = ProgramOptions
  { _iterations       :: Int
  , _dbCreateTables   :: Bool
  , _verbose          :: Bool
  , _inferTypesOnly   :: Bool
  , _outFile          :: Text
  , _debug            :: Bool
  , _inFile           :: Text
  , _skipSemCheck     :: Bool
  , _inliningStrategy :: InliningStrategy
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
  , _inliningStrategy = BreadthFirst
  }

