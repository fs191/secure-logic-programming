-- | Post-processing for privalog programs. Meant to be used after
-- transformation. The result of post-processing is a program that is more
-- similar to the final SecreC code.
module PostProcessing (postProcess) where

import Control.Lens hiding (universe)

import Data.Generics.Uniplate.Data
import Data.List
import Data.Maybe

import Annotation
import DatalogProgram
import Expr
import Rule
import Simplify

-- | Removes all rules that are not called by the goal clause and also removes
-- rules that contain calls to other rules that are not facts.
postProcess :: DatalogProgram -> DatalogProgram
postProcess = removeFalseDP
            . simplifyDP
            . filterGoalRules
            . filterGroundRules

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

simplifyDP :: DatalogProgram -> DatalogProgram
simplifyDP dp = dp & dpRules %~ simplifyRules

simplifyRules :: [Rule] -> [Rule]
simplifyRules rs = map simplifyRule rs

simplifyRule :: Rule -> Rule
simplifyRule r =


    let rName = ruleName r in
    let rBody = simplifyAnds' $ r ^. ruleTail in
    let rArgs = args r in
    let (boundedVars, freeVars) = partition (\z -> z ^. annotation ^. annBound) rArgs in
    let boundedVarNames = concat $ map varNames boundedVars in
    let freeVarNames = concat $ map varNames freeVars in
    let newRuleBody = simplify rBody (boundedVarNames, freeVarNames) in

    let newRuleTail = foldr1 eAnd . nub . filter (not . isAnd) $ newRuleBody in
    --trace ("before: " ++ show (pretty r)) $
    --trace ("after: " ++ show (pretty (rule rName rArgs newRuleTail))) $
    --trace "=====" $
    rule rName rArgs newRuleTail

simplifyAnds' :: Expr -> [Expr]
simplifyAnds' (And _ x y) = simplifyAnds' x <> simplifyAnds' y
simplifyAnds' x = [x]

isAnd :: Expr -> Bool
isAnd And{} = True
isAnd _     = False

-- | Removes facts that always evaluate to False
removeFalseFacts :: [Rule] -> [Rule]
removeFalseFacts = filter (\x -> x ^. ruleTail /= constBool False)
