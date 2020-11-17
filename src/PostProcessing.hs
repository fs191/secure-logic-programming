{-# LANGUAGE FlexibleContexts #-}
-- | Post-processing for privalog programs. Meant to be used after
-- transformation. The result of post-processing is a program that is more
-- similar to the final SecreC code.
module PostProcessing (postProcess) where

import Relude

import Control.Lens hiding (universe)
import Control.Monad.Except

import Data.Generics.Uniplate.Data

import DatalogProgram
import Expr
import ErrorMsg
import Rule

-- | Removes all rules that are not called by the goal clause and also removes
-- rules that contain calls to other rules that are not facts.
postProcess 
  :: (MonadError CompilerException m) 
  => DatalogProgram 
  -> m DatalogProgram
postProcess prog = 
  do
    let prog' = removeFalseDP . filterGoalRules $ filterGroundRules prog
    when (prog' ^. dpRules . to null) $ (return $ DoesNotConverge) *> return ()
    return prog'

-- | Filters out rules that contain predicates that are not facts
filterGroundRules :: DatalogProgram -> DatalogProgram
filterGroundRules dp = dp & dpRules %~ filter fil
  where 
    _facts = _name . view ruleHead <$> extensionalFacts dp
    _name (Pred _ n _) = n
    fil x = all (`elem` _facts) [n | (Pred _ n _) <- universe $ x ^. ruleTail]

-- | Removes all rules that do not get called directly by the goal clause
filterGoalRules :: DatalogProgram -> DatalogProgram
filterGoalRules dp = dp & dpRules .~ _rs
  where _g     = dp ^. dpGoal . _Pred . _2
        _rs    = dp ^.. dpRules . folded . filtered _fil
        _fil x = fromMaybe False $ x ^? ruleHead . _Pred . _2 . to(==_g)

removeFalseDP :: DatalogProgram -> DatalogProgram
removeFalseDP dp = dp & dpRules %~ removeFalseFacts

-- | Removes facts that always evaluate to False
removeFalseFacts :: [Rule] -> [Rule]
removeFalseFacts = filter (\x -> x ^. ruleTail /= constBool False)
