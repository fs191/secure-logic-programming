import ProgramOptions
import Parser
import DatalogProgram

import Optimize(optimize)
import Preprocess(preprocess)
import Transform(deriveAllGroundRules, showAllRules)
import CSVImport(generateDataToDBscript)
import SecreC(generateSecreCscript)

import Control.Monad
import Debug.Trace

main :: IO ()
main = do
  -- command line arguments
  args <- getProgramOptions
  -- the upper bound on the number of search steps
  let n = iterations args
  -- text file containing input Datalog program
  let inFileName = inFile args
  -- text file containing output SecreC program
  let outFilePath = outFile args

  -- parse the input datalog program
  program <- parseDatalogFromFile inFileName


  -- Verify that parser works correctly
  --traceIO $ (show facts)
  --traceIO $ (show rules)
  --traceIO $ (show goal)
  --traceIO $ "--------------------------------------------------------"

  -- create a Sharemind script that can be used to upload the tables used in given program
  -- WARNING: this is used for testing only, do not apply it to actual private data!
  when (dbCreateTables args) $ do
      createdb <- generateDataToDBscript $ facts program
      let outFileDir  = reverse $ dropWhile (/= '/') (reverse outFilePath)
      let outFileName = reverse $ takeWhile (/= '/') (reverse outFilePath)

      let createdbPath = outFileDir ++ "createdb_" ++ outFileName
      writeFile createdbPath createdb

  -- apply Magic Sets or some alternative preprocessing here
  let program' = preprocess program
      facts' = facts program'
      rules' = rules program'
      goal'  = goal program'

  -- using rules, generate all facts that we get in up to 'n' steps of rule application
  -- since we are working with symbolic data, our "facts" are actually "ground rules", i.e. rules without free variables,
  -- but they may have bounded variables that will be taken from the datbase
  -- the output is if type 'M.Map PName PMap' where:
  -- - PName is the predicate name, e.g. 'buys'
  -- - PMap maps 'arguments of the predicate' to 'RHS of the corresponding ground rule'
  let groundRules = deriveAllGroundRules facts' rules' n

  -- we do all optimizations of computation (like constant propagation) here
  let optimizedGroundRules = optimize groundRules

  -- show the transformation result
  traceIO $ showAllRules optimizedGroundRules

  -- we can output either only yes/no answer, or also valuations of free variables
  let boolOnly = (outputOnlyBool args)
  let optimizedProgram = makeProgram optimizedGroundRules rules' goal'
  let secrec = generateSecreCscript boolOnly optimizedProgram

  -- Output the results
  if outFilePath /= ""
     then writeFile outFilePath secrec
     else putStrLn secrec


