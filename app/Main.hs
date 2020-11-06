{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Relude

import ErrorMsg
import Parser.DatalogParser
import Language.SecreC

import Control.Exception
import Data.Text.Prettyprint.Doc
import qualified Data.Text as T

import Translator hiding (_debug)
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

main :: IO ()
main = 
  do
    let act = do
          args <- getProgramOptions
          -- Text file containing input Datalog program
          let inFileName = _inFile args
          -- Text file containing output SecreC program
          let outFilePath = _outFile args
          let _ite = _iterations args
          let inferTypesOnly = _inferTypesOnly args
          let debug = _debug args

          -- parse the input datalog program
          program' <- parseDatalogFromFile $ inFileName 
          let program = case program' of
                Left ex -> throw ex
                Right x -> x

          let conf = TranslatorConfig _ite debug
          tr <- runExceptT $ process conf program
          let tr' = either throw id tr

          let sc = secrecCode tr'

          let output = show $ if inferTypesOnly
              then pretty tr'
              else pretty sc

          -- create a Sharemind script that can be used to upload the tables used in given program
          -- WARNING: this is used for testing only, do not apply it to actual private data!
          when (_dbCreateTables args) $ do
              createdb <- csvImportCode program
              let createdbStr = show . pretty $ createdb
              let outFileDir  = T.reverse $ T.dropWhile (/= '/') (T.reverse outFilePath)
              let outFileName = T.reverse $ T.takeWhile (/= '/') (T.reverse outFilePath)

              let createdbPath = outFileDir <> "createdb_" <> outFileName
              writeFile (toString createdbPath) createdbStr

          -- Output the results
          writeFileText (toString outFilePath) output
    res <- try act
    case res of
      Left (ex :: SomeException) -> 
        do
          putStrLn $ show ex
          putStrLn $ prettyCallStack callStack
      Right _ -> return ()
