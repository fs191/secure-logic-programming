module PostProcessing (postProcess) where

import Control.Lens hiding (universe)

import Data.Generics.Uniplate.Data
import Data.Maybe

import DatalogProgram
import Expr
import Rule

postProcess :: DatalogProgram -> DatalogProgram
postProcess = filterGoalRules
            . filterGroundRules

-- | Filters out rules that contain predicates that are not facts
filterGroundRules :: DatalogProgram -> DatalogProgram
filterGroundRules dp = dp & dpRules %~ filter fil
  where 
    _facts = _name . view ruleHead <$> extensionalFacts dp
    _name (Pred _ n _) = n
    fil x = all (`elem` _facts) [n | (Pred _ n _) <- universe $ x ^. ruleTail]

filterGoalRules :: DatalogProgram -> DatalogProgram
filterGoalRules dp = dp & dpRules .~ _rs
  where _g     = dp ^. dpGoal . _Pred . _2
        _rs    = dp ^.. dpRules . folded . filtered _fil
        _fil x = fromMaybe False $ x ^? ruleHead . _Pred . _2 . to(==_g)

