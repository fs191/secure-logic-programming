import Transform
import ProgramOptions

import Control.Monad

main :: IO ()
main = do
  args <- getProgramOptions
  test args
