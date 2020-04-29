module Optimize
  ( optimize
  ) where

---------------------------------------------------------
---- Optimization of intermediate representation
---------------------------------------------------------

import qualified Data.Map as M
import qualified Data.Set as S

import Aexpr
import ErrorMsg
import Rule
import DatalogProgram

optimize :: (LogicProgram a) => a -> a
optimize = id
