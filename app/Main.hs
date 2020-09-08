{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import ProgramOptions
import OptParse
import ErrorMsg
import Parser.DatalogParser
import Language.SecreC

import Control.Exception
import Control.Monad hiding (ap)
import Data.Text.Prettyprint.Doc

import Transform
import PreProcessing
import PostProcessing
import TypeInference
import Adornment
import DatalogProgram

main :: IO ()
main = 
  do
    res <- try $ do
      args <- getProgramOptions
      -- Text file containing input Datalog program
      let inFileName = _inFile args
      -- Text file containing output SecreC program
      let outFilePath = _outFile args
      let _ite = _iterations args
      let inferTypesOnly = _inferTypesOnly args

      -- parse the input datalog program
      program' <- try $ parseDatalogFromFile inFileName 
        :: IO (Either IOException DatalogProgram)
      let program = case program' of
            Left ex -> throw $ CannotReadFile inFileName ex
            Right x -> x

      pp   <- return $ preProcess program
      ap   <- return $ adornProgram pp
      tf   <- return $ deriveAllGroundRules _ite ap
      post <- return $ 
        if inferTypesOnly
          then tf
          else postProcess tf
      ti   <- return $ typeInference post
      sc   <- return $ secrecCode ti

      let output = show $ if inferTypesOnly
          then pretty ti
          else pretty sc

      -- create a Sharemind script that can be used to upload the tables used in given program
      -- WARNING: this is used for testing only, do not apply it to actual private data!
      when (_dbCreateTables args) $ do
          createdb <- csvImportCode program
          let createdbStr = show . pretty $ createdb
          let outFileDir  = reverse $ dropWhile (/= '/') (reverse outFilePath)
          let outFileName = reverse $ takeWhile (/= '/') (reverse outFilePath)

          let createdbPath = outFileDir ++ "createdb_" ++ outFileName
          writeFile createdbPath createdbStr

      -- Output the results
      writeFile outFilePath output
    case res of
      Left ex -> putStrLn $ show (ex :: CompilerException)
      Right _ -> return ()

