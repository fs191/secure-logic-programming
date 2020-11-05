{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module OptParse
  ( getProgramOptions
  , _iterations
  , _dbCreateTables
  , _verbose
  , _inferTypesOnly
  , _inFile
  , _outFile
  , _debug
  ) where

import Relude

import Options.Applicative

data ProgramOptions = ProgramOptions
  { _iterations     :: Int
  , _dbCreateTables :: Bool
  , _verbose        :: Bool
  , _inferTypesOnly :: Bool
  , _outFile        :: Text
  , _debug          :: Bool
  , _inFile         :: Text
  }

programArgs :: Parser ProgramOptions
programArgs = ProgramOptions
  <$> (option auto $ mconcat
        [ short 'n'
        , long "iterations"
        , help "Specifies the maximum depth of recursion for the transformation\
                \ stage. Larger value means that compilation will take longer,\ 
                \ but at lower values the SecreC program might only give a\
                \ partial answer."
        , value 2
        , hidden
        ])
      
  <*> (switch $ mconcat
        [ long "db-create-tables"
        , hidden
        , help "Create the required tables in the database using the data in input files\n \
                \ the script is written into file createdb_XXX where XXX is the specified output file."
        ])
  <*> (switch $ mconcat
        [ short 'v'
        , long "verbose"
        , help "Produce verbose output."
        , hidden
        ])
  <*> (switch $ mconcat
        [ short 't'
        , long "only-types"
        , help "Outputs Privalog program with inferred types."
        , hidden
        ])
  <*> (strOption $ mconcat
        [ short 'o'
        , long "output"
        , help "Specify output file path. Outputs into 'out' by default."
        , value "out"
        ])
  <*> (switch $ mconcat
        [ long "debug"
        , help "Prints all intermediate results. WARNING: Will produce a lot of output."
        , hidden
        ])
  <*> (strArgument $ mconcat
        [ metavar "INPUT"
        ])

getProgramOptions :: IO ProgramOptions
getProgramOptions = execParser opts
  where
    opts = info (programArgs <**> helper)
      (fullDesc
      <> progDesc "Transforms a Privalog program to a privacy-preserving SecreC program.\n\
                  \ WARNING: the program is still under development."
      <> header "Privacy-preserving logic programming")
