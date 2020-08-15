module ProgramOptions
  ( ProgramOptions(..)
  ) where

import Options.Applicative
import Data.Semigroup ((<>))

data ProgramOptions = ProgramOptions
  { _iterations     :: Int
  , _dbCreateTables :: Bool
  , _inFile         :: String
  , _outFile        :: String
  }

