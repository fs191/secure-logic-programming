module Preprocess where

---------------------------------------------------------
---- Preprocessing (such as Magic Sets algorithm)
---------------------------------------------------------

import qualified Data.Map as M
import qualified Data.Set as S

import Aexpr
import ErrorMsg
import Rule

preprocess facts rules goal =
    --TODO implement preprocessing algorithm here
    (facts,rules,goal)
