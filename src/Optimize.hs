module Optimize where

---------------------------------------------------------
---- Optimization of intermediate representation
---------------------------------------------------------

import qualified Data.Map as M
import qualified Data.Set as S

import Aexpr
import ErrorMsg
import Rule

optimize :: (M.Map PName PMap) -> (M.Map PName PMap)
optimize factMap =
    --TODO implement optimization here
    factMap
