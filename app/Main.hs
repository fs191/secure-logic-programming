{-# LANGUAGE ScopedTypeVariables #-}

import Relude

import Parser.DatalogParser
import Language.SecreC

import Data.Text.Prettyprint.Doc
import qualified Data.Text as T

import Translator
import Options.Applicative

import ErrorMsg
import ProgramOptions
import DatalogProgram

programArgs :: Parser ProgramOptions
programArgs = ProgramOptions
  <$> option auto (mconcat
        [ short 'n'
        , long "iterations"
        , help "Specifies the maximum depth of recursion for the transformation\
                \ stage. Larger value means that compilation will take longer,\ 
                \ but at lower values the SecreC program might only give a\
                \ partial answer."
        , value 2
        , hidden
        ])
  <*> switch (mconcat
        [ long "db-create-tables"
        , hidden
        , help "Create the required tables in the database using the data in input files\n \
                \ the script is written into file createdb_XXX where XXX is the specified output file."
        ])
  <*> switch (mconcat
        [ short 'v'
        , long "verbose"
        , help "Produce verbose output."
        , hidden
        ])
  <*> switch (mconcat
        [ short 't'
        , long "only-types"
        , help "Outputs Privalog program with inferred types."
        , hidden
        ])
  <*> strOption (mconcat
        [ short 'o'
        , long "output"
        , help "Specify output file path. Outputs into 'out' by default."
        , value "out"
        ])
  <*> switch (mconcat
        [ short 'd'
        , long "debug"
        , help "Prints debug information."
        , hidden
        ])
  <*> strArgument (mconcat
        [ metavar "INPUT"
        ])
  <*> switch (mconcat
         [ long "skip-semantics-check"
         , help "Skips the semantics check"
         , hidden
         ])
  <*> option (maybeReader $ stratReader . toText) (mconcat
        [ short 's'
        , long "inlining-strategy"
        , help "Specify the inlining strategy. Possible values:\n\
        \ bfs, dfs, gr"
        , value BreadthFirst
        ])

stratReader :: Text -> Maybe InliningStrategy
stratReader "bfs" = Just BreadthFirst
stratReader "dfs" = Just DepthFirst
stratReader "gr" = Just FullGround
stratReader _ = Nothing

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
    args <- getProgramOptions
    -- Text file containing input Datalog program
    let inFileName = _inFile args
    -- Text file containing output SecreC program
    let outFilePath = _outFile args
    let _ite = _iterations args
    let onlyTypes = _inferTypesOnly args

    -- parse the input datalog program
    source <- liftIO . readFileText $ toString inFileName
    let program' = parseDatalog inFileName source
    program <- case program' of
          Left ex -> 
            do
              print $ errorMsg source ex
              exitFailure
          Right x -> return x

    tr <- runExceptT $ runReaderT process args :: IO (Either CompilerException DatalogProgram)
    case tr of
      Left ex -> 
        do
          print $ errorMsg source ex
          exitFailure
      Right tr' -> 
        do
          let sc = secrecCode tr'

          let output = show $ if onlyTypes
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

