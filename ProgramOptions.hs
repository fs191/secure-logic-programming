module ProgramOptions
  (ProgramOptions(..), getProgramOptions)
  where

import Options.Applicative
import Data.Semigroup ((<>))

data ProgramOptions
  = ProgramOptions {
    iterations :: Int
  }

programArgs :: Parser ProgramOptions
programArgs = ProgramOptions
  <$> option auto (short 'n' <> long "iterations" <> value 100 <> help "Specify the maximum number of iterations")

getProgramOptions :: IO ProgramOptions
getProgramOptions = execParser opts
  where
    opts = info (programArgs <**> helper)
      (fullDesc
      <> progDesc "Transforms a Datalog program to a privacy-preserving SecreC program.\
                  \ WARNING: the program is under development yet."
      <> header "Privacy-preserving logic programming")
