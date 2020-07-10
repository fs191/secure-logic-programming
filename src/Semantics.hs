module Semantics 
  (
  ) where

import DatalogProgram

data SemanticsException 
  = SemanticsException

annotateProgram :: DatalogProgram -> Either SemanticsException DatalogProgram
annotateProgram dp = undefined

