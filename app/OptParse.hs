module OptParse
  ( getProgramOptions
  ) where

import Options.Applicative
import Data.Semigroup ((<>))

import ProgramOptions

programArgs :: Parser ProgramOptions
programArgs = ProgramOptions
  <$> strArgument (metavar "INPUT_PATH" <> help "input file containing initial datalog program")
  <*> strOption (metavar "OUTPUT_PATH" <> value "" <> help "output file containing resulting SecreC program")
  <*> option auto (short 'n' <> long "iterations" <> value 10 <> help "specify the maximum number of iterations")
  <*> switch (long "db-create-tables"
              <> hidden
              <> help "Create the required tables in the database using the data in input files\n \
                       \ the script is written into file createdb_XXX where XXX is the specified output file")
  <*> switch (long "yes-no-only"
              <> hidden
              <> help "Output only a yes/no result, even if the goal contains free variables")

getProgramOptions :: IO ProgramOptions
getProgramOptions = execParser opts
  where
    opts = info (programArgs <**> helper)
      (fullDesc
      <> progDesc "Transforms a Datalog program to a privacy-preserving SecreC program.\n \
                  \ WARNING: the program is still under development."
      <> header "Privacy-preserving logic programming")
