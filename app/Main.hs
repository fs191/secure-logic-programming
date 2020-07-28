import ProgramOptions
import OptParse
import Parser.DatalogParser
import Language.SecreC

--import CSVImport(generateDataToDBscript)

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

    -- Output the results
    if outFilePath /= ""
       then writeFile outFilePath secrec
       else putStrLn secrec

