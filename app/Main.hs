{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Relude

import ProgramOptions
import OptParse
import ErrorMsg
import Parser.DatalogParser
import Language.SecreC

import Control.Exception
import Data.Text.Prettyprint.Doc
import qualified Data.Text as T

import DatalogProgram
import Translator

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

          -- parse the input datalog program
          program' <- try . parseDatalogFromFile $ inFileName 
            :: IO (Either IOException DatalogProgram)
          let program = case program' of
                Left ex -> throw $ CannotReadFile inFileName ex
                Right x -> x

          let conf = TranslatorConfig _ite
          tr <- process conf program
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
              writeFile (show createdbPath) createdbStr

          -- Output the results
          writeFileText (show outFilePath) output
    res <- try act
    case res of
      Left (ex :: SomeException) -> 
        do
          putStrLn $ show ex
          putStrLn $ prettyCallStack callStack
      Right _ -> return ()

