import ProgramOptions
import OptParse
import Parser.DatalogParser
import Translator

import CSVImport(generateDataToDBscript)

import Control.Monad

main :: IO ()
main = do
  -- command line arguments
  args <- getProgramOptions
  -- text file containing input Datalog program
  let inFileName = _inFile args
  -- text file containing output SecreC program
  let outFilePath = _outFile args

  -- parse the input datalog program
  program <- parseDatalogFromFile inFileName

  -- Verify that parser works correctly
  --traceIO $ (show facts)
  --traceIO $ (show rules)
  --traceIO $ (show goal)
  --traceIO $ "--------------------------------------------------------"

  -- create a Sharemind script that can be used to upload the tables used in given program
  -- WARNING: this is used for testing only, do not apply it to actual private data!
  when (_dbCreateTables args) $ do
      createdb <- generateDataToDBscript program
      let outFileDir  = reverse $ dropWhile (/= '/') (reverse outFilePath)
      let outFileName = reverse $ takeWhile (/= '/') (reverse outFilePath)

      let createdbPath = outFileDir ++ "createdb_" ++ outFileName
      writeFile createdbPath createdb


  -- we can output either only yes/no answer, or also valuations of free variables
  let secrec = evalTranslator
        $ withBoolOnly (_outputOnlyBool args)
        $ withIterations (_iterations args)
        $ translate =<< process program

  -- Output the results
  if outFilePath /= ""
     then writeFile outFilePath secrec
     else putStrLn secrec


