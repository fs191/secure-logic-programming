module PostProcessing (filterGroundRules) where

import Control.Lens hiding (universe)

import Data.Generics.Uniplate.Data

import DatalogProgram
import Expr
import Rule

-- | Filters out rules that contain predicates that are not facts
filterGroundRules :: DatalogProgram -> DatalogProgram
filterGroundRules dp = dp & dpRules %~ filter fil
  where 
    _facts = _name . view ruleHead <$> extensionalFacts dp
    _name (Pred _ n _) = n
    fil x = all (`elem` _facts) [n | (Pred _ n _) <- universe $ x ^. ruleTail]

