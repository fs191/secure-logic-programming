{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import ProgramOptions
import OptParse
import Parser.DatalogParser
import Language.SecreC

--import CSVImport(generateDataToDBscript)

import Control.Monad hiding (ap)
import Data.Text.Prettyprint.Doc
import System.Log.Logger

import Transform
import PreProcessing
import PostProcessing
import TypeInference
import Adornment

main :: IO ()
main = 
  do
    args <- getProgramOptions
    -- Text file containing input Datalog program
    let inFileName = _inFile args
    -- Text file containing output SecreC program
    let outFilePath = _outFile args
    let _ite = _iterations args
    -- Set logging level
    when (_verbose args) $
      updateGlobalLogger rootLoggerName (setLevel DEBUG)

    -- parse the input datalog program
    infoM rootLoggerName $ "Parsing " ++ inFileName
    program <- parseDatalogFromFile inFileName

    pp <- (return $ preProcess program)
    ap <- (return $ adornProgram pp)
    tf <- (return $ deriveAllGroundRules _ite ap)
    post <- (return $ postProcess tf)
    ti <- (return $ typeInference post)
    sc <- (return $ secrecCode ti)

    let secrec = show $ pretty sc

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
    writeFile outFilePath secrec

