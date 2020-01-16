import Transform
import ProgramOptions

import Control.Monad

main :: IO ()
main = do
  args <- getProgramOptions
  let n = iterations args
  let infile = inFile args
  test infile n
