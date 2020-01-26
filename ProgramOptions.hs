module ProgramOptions
  (ProgramOptions(..), getProgramOptions)
  where

import Options.Applicative
import Data.Semigroup ((<>))

data ProgramOptions
  = ProgramOptions {
    inFile     :: FilePath,
    iterations :: Int,
    dbCreateTables :: Bool
  }

programArgs :: Parser ProgramOptions
programArgs = ProgramOptions
  <$> strArgument (metavar "DATALOG PROGRAM" <> help "input file containing datalog program")
  <*> option auto (short 'n' <> long "iterations" <> value 10 <> help "specify the maximum number of iterations")
  <*> switch (long "db-create-tables" <> hidden <> help "Create the required tables in the database using the data in input files")

getProgramOptions :: IO ProgramOptions
getProgramOptions = execParser opts
  where
    opts = info (programArgs <**> helper)
      (fullDesc
      <> progDesc "Transforms a Datalog program to a privacy-preserving SecreC program.\
                  \ WARNING: the program is under development yet."
      <> header "Privacy-preserving logic programming")
