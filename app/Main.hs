import ProgramOptions
import OptParse
import Parser.DatalogParser
import Language.SecreC

--import CSVImport(generateDataToDBscript)

import Control.Monad
import Data.Text.Prettyprint.Doc
import Transform
import PreProcessing
import PostProcessing
import TypeInference
import Adornment

main :: IO ()
main = do
  -- command line arguments
  args <- getProgramOptions
  runWithOptions args

runWithOptions :: ProgramOptions -> IO ()
runWithOptions args =
  do
    -- text file containing input Datalog program
    let inFileName = _inFile args
    -- text file containing output SecreC program
    let outFilePath = _outFile args
    let _ite = _iterations args

    let pipeline 
          = secrecCode
          . typeInference
          . postProcess
          . deriveAllGroundRules _ite
          . adornProgram
          . preProcess

    -- parse the input datalog program
    program <- parseDatalogFromFile inFileName
    let secrec = show . pretty $ pipeline program

    -- create a Sharemind script that can be used to upload the tables used in given program
    -- WARNING: this is used for testing only, do not apply it to actual private data!
    when (_dbCreateTables args) $ do
        createdb <- csvImportCode program
        let createdbStr = show . pretty $ createdb
        let outFileDir  = reverse $ dropWhile (/= '/') (reverse outFilePath)
        let outFileName = reverse $ takeWhile (/= '/') (reverse outFilePath)

        let createdbPath = outFileDir ++ "createdb_" ++ outFileName
        writeFile createdbPath createdbStr
        putStrLn $ "Wrote database generation SecreC script into " ++ createdbPath

    -- Output the results
    if outFilePath /= ""
       then do
            writeFile outFilePath secrec
            putStrLn $ "Wrote privacy-preserving computation SecreC script into " ++ outFilePath
       else putStrLn secrec

