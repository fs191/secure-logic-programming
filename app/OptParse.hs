{-# LANGUAGE ApplicativeDo #-}

module OptParse
  ( getProgramOptions
  ) where

import Options.Applicative

import ProgramOptions

programArgs :: Parser ProgramOptions
programArgs =
  do
    iterations <- option auto $ mconcat
      [ short 'n'
      , long "iterations"
      , help "Specifies the maximum depth of recursion for the transformation\
              \ stage. Larger value means that compilation will take longer,\ 
              \ but at lower values the SecreC program might only give a\
              \ partial answer"
      , value 2
      , hidden
      ]
      
    createTables <- switch $ mconcat
      [ long "db-create-tables"
      , hidden
      , help "Create the required tables in the database using the data in input files\n \
              \ the script is written into file createdb_XXX where XXX is the specified output file"
      , hidden
      ]
    verbose <- switch $ mconcat
      [ short 'v'
      , long "verbose"
      , help "Produce verbose output"
      , hidden
      ]
    outFile <- strOption $ mconcat
      [ short 'o'
      , long "output"
      , help "Specify output file path. Outputs into out.sc by default."
      , value "out.sc"
      , hidden
      ]
    inFile <- strArgument $ mconcat
      [ metavar "INPUT"
      , help "The input Privalog file"
      ]
    pure $ ProgramOptions
      { _iterations     = iterations
      , _dbCreateTables = createTables
      , _verbose        = verbose
      , _inFile         = inFile
      , _outFile        = outFile
      }

getProgramOptions :: IO ProgramOptions
getProgramOptions = execParser opts
  where
    opts = info (programArgs <**> helper)
      (fullDesc
      <> progDesc "Transforms a Privalog program to a privacy-preserving SecreC program.\n \
                  \ WARNING: the program is still under development."
      <> header "Privacy-preserving logic programming")
