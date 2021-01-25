{-# LANGUAGE FlexibleContexts #-}
module Main where

import Relude
import Data.Text.Prettyprint.Doc

import Options.Applicative

import Parser.DatalogParser
import Utils.TableGen

optParse :: Parser CsvImportOptions
optParse = CsvImportOptions 
  <$> strArgument (mconcat 
        [ metavar "INPUT"
        , help "Path to Privalog program"
        ])
  <*> strArgument (mconcat 
        [ metavar "OUTPUT"
        , value "createdb_out"
        , help "Output file path"
        ])
  <*> strOption (mconcat
        [ long "database"
        , short 'd'
        , value "examples/database"
        , help "path to a directory containing the database .csv files"
        ])
  

main :: IO ()
main =
  do
    let opts = info (optParse <**> helper) $ mconcat
          [ fullDesc
          , progDesc "Generate a SecreC program for setting up the database"
          ]
    args <- execParser opts
    let inFilePath = _poInFilePath args
    let outFilePath = _poOutFilePath args
    program' <- parseDatalogFromFile inFilePath
    let program = either (error . show) id program'
    createdb <- runReaderT (csvImportCode program) args
    let createdbStr = show . pretty $ createdb
    writeFile (toString outFilePath) createdbStr


