module Preprocess (preprocess) where

---------------------------------------------------------
---- Preprocessing (such as Magic Sets algorithm)
---------------------------------------------------------

import qualified Data.Map as M
import qualified Data.Set as S

import Aexpr
import ErrorMsg
import Rule
import DatalogProgram

preprocess :: DatalogProgram -> DatalogProgram
preprocess = id --TODO implement preprocessing algorithm here
