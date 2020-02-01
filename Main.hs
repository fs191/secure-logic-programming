import ProgramOptions
import Parser
import Transform
import CSVImport
import SecreC

import Control.Monad
import Debug.Trace

main :: IO ()
main = do
  args <- getProgramOptions

  let n = iterations args
  let inFileName = inFile args
  let outFilePath = outFile args

  (database,rules,goal) <- parseDatalogFromFile inFileName

  traceIO $ (show database)
  traceIO $ (show rules)
  traceIO $ (show goal)
  traceIO $ "--------------------------------------------------------"

  when (dbCreateTables args) $ do
      createdb <- generateDataToDBscript database
      let outFileDir  = reverse $ dropWhile (/= '/') (reverse outFilePath)
      let outFileName = reverse $ takeWhile (/= '/') (reverse outFilePath)

      let createdbPath = outFileDir ++ "createdb_" ++ outFileName
      writeFile createdbPath createdb

  let expandedFacts = runIteration database rules 0 n

  traceIO $ showResult expandedFacts
  traceIO $ "--------------------------------------------------------"

  let secrec = generateSecreCscript expandedFacts goal

  -- Output the results
  if outFilePath /= ""
     then writeFile outFilePath secrec
     else putStrLn secrec
