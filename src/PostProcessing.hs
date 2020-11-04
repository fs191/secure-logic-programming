-- | Post-processing for privalog programs. Meant to be used after
-- transformation. The result of post-processing is a program that is more
-- similar to the final SecreC code.
module PostProcessing (postProcess) where

import Data.Generics.Uniplate.Operations as U
import Control.Lens hiding (universe)

import Data.Generics.Uniplate.Data
import Data.List
import Data.Maybe
import Data.Hashable
import qualified Data.Set as S

import Annotation
import DatalogProgram
import Expr
import Rule
import Simplify

import Debug.Trace

-- | Removes all rules that are not called by the goal clause and also removes
-- rules that contain calls to other rules that are not facts.
postProcess :: DatalogProgram -> DatalogProgram
postProcess = removeFalseDP
            . simplifyDP
            . mergeRulesDP
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

mergeRulesDP :: DatalogProgram -> DatalogProgram
mergeRulesDP dp = dp & dpRules %~ mergeRules dp

refreshRulesDP :: DatalogProgram -> DatalogProgram
refreshRulesDP dp = dp & dpRules . traversed %~ refreshRule "X_"

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

isPredicate :: Expr -> Bool
isPredicate Pred{} = True
isPredicate _      = False

isComparison :: Expr -> Bool
isComparison Eq{}  = True
isComparison Neq{} = True
isComparison Le{} = True
isComparison Lt{} = True
isComparison Ge{} = True
isComparison Gt{} = True
isComparison _    = False

-- | Removes facts that always evaluate to False
removeFalseFacts :: [Rule] -> [Rule]
removeFalseFacts = filter (\x -> x ^. ruleTail /= constBool False)

-- | Merges similar rules into one by introducing disjunctions
mergeRules :: DatalogProgram -> [Rule] -> [Rule]
mergeRules dp rs =
    let rs0 = map (prepareRule dp) rs in
    let rs1 = map (refreshRule "X_") rs0 in
    let rs2 = simplifyRules rs1 in
    mergeMatchingRules rs2

prepareRule :: DatalogProgram -> Rule -> Rule
prepareRule dp r = r & ruleTail %~ (\ttl -> foldr1 eAnd $ zipWith (extendEDB dp) (simplifyAnds' ttl) [1..])

-- fills all EDB predicates with fresh variables
-- while it makes rules longer, it is easier to optimize them in this form
extendEDB :: DatalogProgram -> Expr -> Int -> Expr
extendEDB dp e j =
    if isPredicate e then
        let pname   = e ^. predName in
        let oldArgs = predicateVars e in
        let newArgs = map (\i -> var ("T_" ++ show j ++ "_" ++ show i)) [1..length oldArgs] in
        let asgns   = zipWith (\x (Var ann y) -> eUn x (Var (ann & annBound .~ True) y)) oldArgs newArgs in
        foldl eAnd (predicate pname newArgs) asgns
    else e

mergeMatchingRules :: [Rule] -> [Rule]
mergeMatchingRules [] = []
mergeMatchingRules (r:rs) =
    let rs' = mergeMatchingRulePairs rs r in
    last rs' : mergeMatchingRules (init rs')

mergeMatchingRulePairs :: [Rule] -> Rule -> [Rule]
mergeMatchingRulePairs [] r = [r]
mergeMatchingRulePairs (r1 : rs) r2 =
    let rs' = mergeTwoMatchingRules r1 r2 in
    tail rs' ++ mergeMatchingRulePairs rs (head rs')


-- TODO we can precompute a lot of things just once
mergeTwoMatchingRules :: Rule -> Rule -> [Rule]
mergeTwoMatchingRules r1 r2 =
   -- 1. check that the rules have the same head
   if r1 ^. ruleHead /= r2 ^. ruleHead then
       [r1,r2]
   else
       let es1       = simplifyAnds' (r1 ^. ruleTail) in
       let (ps1,rs1) = partition isPredicate es1 in
       let (cs1,as1) = partition isComparison rs1 in
       let hps1 = map (hash . show) ps1 in
       let has1 = map (hash . show) as1 in
       let hcs1 = map (hash . show) cs1 in

       let es2       = simplifyAnds' (r2 ^. ruleTail) in
       let (ps2,rs2) = partition isPredicate es2 in
       let (cs2,as2) = partition isComparison rs2 in
       let hps2 = map (hash . show) ps2 in
       let has2 = map (hash . show) as2 in
       let hcs2 = map (hash . show) cs2 in

       -- 1. check that the rules have the same EDB inputs
       if S.fromList hps1 /= S.fromList hps2 then
           [r1,r2]

       -- 2. check that the rules have the same non-deterministic assignments
       -- TODO try to generalize it
       else if S.fromList has1 /= S.fromList has2 then
           [r1,r2]

       -- 3. extract comparisons that are identical in both rules
       else
           let hs = S.intersection (S.fromList hcs1) (S.fromList hcs2) in
           let cs_common = map fst . filter (\(_,hx) -> S.member hx hs)     $ zip cs1 hcs1 in
           let cs_from1  = map fst . filter (\(_,hx) -> not (S.member hx hs)) $ zip cs1 hcs1 in
           let cs_from2  = map fst . filter (\(_,hx) -> not (S.member hx hs)) $ zip cs2 hcs2 in

           -- if merging does not make sense, leave the rules as they are
           if (length cs_common + length cs_from1 + length cs_from2) >= (length cs1 + length cs2) then
               [r1,r2]
           else
               let rHead = r1 ^. ruleHead in
               let rTail = foldr1 eAnd $ ps1 ++ as1 ++ cs_common ++ [eOr (foldr1 eAnd cs_from1) (foldr1 eAnd cs_from2)] in
               [Rule rHead rTail]

