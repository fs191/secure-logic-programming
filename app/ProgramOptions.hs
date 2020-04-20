module ProgramOptions
  ( ProgramOptions(..)
  ) where

import Options.Applicative
import Data.Semigroup ((<>))

data ProgramOptions = ProgramOptions
  { _inFile         :: String
  , _outFile        :: String
  , _iterations     :: Int
  , _dbCreateTables :: Bool
  , _outputOnlyBool :: Bool
  }

