-- | Post-processing for privalog programs. Meant to be used after
-- transformation. The result of post-processing is a program that is more
-- similar to the final SecreC code.
module PostProcessing (postProcess) where

import Data.Generics.Uniplate.Operations as U
import Control.Lens hiding (universe)

import Data.Generics.Uniplate.Data
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Hashable
import qualified Data.Set as S

import Annotation
import DatalogProgram
import Expr
import Rule
import Simplify
import Substitution

import Data.Text.Prettyprint.Doc
import Debug.Trace

-- | Removes all rules that are not called by the goal clause and also removes
-- rules that contain calls to other rules that are not facts.
postProcess :: DatalogProgram -> DatalogProgram
postProcess = removeFalseDP
            . foldChooseDP
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

-- | Merges similar rules into one by introducing disjunctions
mergeRulesDP :: DatalogProgram -> DatalogProgram
mergeRulesDP dp = dp & dpRules %~ map (prepareRule dp)
                     & dpRules %~ map (refreshRule "X_")
                     & dpRules %~ simplifyRules
                     & dpRules %~ mergeMatchingRules

foldChooseDP :: DatalogProgram -> DatalogProgram
foldChooseDP dp = dp & dpRules . traversed . ruleTail %~ (\ttl -> foldr1 eAnd $ map foldChoose (simplifyAnds' ttl))

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

isGroundPredicate :: Expr -> Bool
isGroundPredicate Eq{}  = True
isGroundPredicate Neq{} = True
isGroundPredicate Le{} = True
isGroundPredicate Lt{} = True
isGroundPredicate Ge{} = True
isGroundPredicate Gt{} = True
-- by assumption, OR is applied only to ground predicates
isGroundPredicate Or{} = True
isGroundPredicate ConstBool{} = True
isGroundPredicate _    = False

-- | Removes facts that always evaluate to False
removeFalseFacts :: [Rule] -> [Rule]
removeFalseFacts = filter (\x -> x ^. ruleTail /= constBool False)


prepareRule :: DatalogProgram -> Rule -> Rule
prepareRule dp r = r & ruleTail %~ extendEDB_DP dp
                     & ruleTail %~ reorderTail

-- order the tail expressions as Predicates - Assignments - Comparisons
reorderTail :: Expr -> Expr
reorderTail ttl =
       let (ps,as,cs) = splitTail ttl in
       foldr1 eAnd $ ps ++ as ++ cs

-- split the tail expressions to Predicates - Assignments - Comparisons
splitTail :: Expr -> ([Expr], [Expr], [Expr])
splitTail ttl =
       let es      = simplifyAnds' ttl in
       let (ps,rs) = partition isPredicate es in
       let (cs,as) = partition isGroundPredicate rs in
       (ps,as,cs)

-- fills all EDB predicates with fresh variables
-- while it makes rules longer, it is easier to optimize them in this form
extendEDB_DP :: DatalogProgram -> Expr -> Expr
extendEDB_DP dp ttl = foldr1 eAnd $ zipWith (extendEDB dp) (simplifyAnds' ttl) [1..]

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
mergeMatchingRulePairs (r2 : rs) r1 =
    let rs' = mergeTwoMatchingRules r1 r2 in
    tail rs' ++ mergeMatchingRulePairs rs (head rs')


-- TODO we can precompute a lot of things just once
mergeTwoMatchingRules :: Rule -> Rule -> [Rule]
mergeTwoMatchingRules r1 r2 =
   -- 1. check that the rules have the same head
   if (hash . show) (r1 ^. ruleHead) /= (hash . show) (r2 ^. ruleHead) then
       [r1,r2]
   else
       let (ps1,as1,cs1) = splitTail (r1 ^. ruleTail) in
       let hps1 = map (hash . show) ps1 in
       let has1 = map (hash . show) as1 in
       let hcs1 = map (hash . show) cs1 in

       let (ps2,as2,cs2) = splitTail (r2 ^. ruleTail) in
       let hps2 = map (hash . show) ps2 in
       let has2 = map (hash . show) as2 in
       let hcs2 = map (hash . show) cs2 in

       -- 2. check that the rules have the same EDB inputs
       -- together, 1 and 2 ensure that the rules operate on the same input state
       if S.fromList hps1 /= S.fromList hps2 then
           [r1,r2]

       -- 3. extract comparisons and assignments that are identical in both rules
       else
           let ps = ps1 in

           let hcs = S.intersection (S.fromList hcs1) (S.fromList hcs2) in
           let cs_common = map fst . filter (\(_,hx) -> S.member hx hcs)     $ zip cs1 hcs1 in
           let cs_from1  = map fst . filter (\(_,hx) -> not (S.member hx hcs)) $ zip cs1 hcs1 in
           let cs_from2  = map fst . filter (\(_,hx) -> not (S.member hx hcs)) $ zip cs2 hcs2 in

           let has = S.intersection (S.fromList has1) (S.fromList has2) in
           let as_common = map fst . filter (\(_,hx) -> S.member hx has)     $ zip as1 has1 in
           let as_from1  = map fst . filter (\(_,hx) -> not (S.member hx has)) $ zip as1 has1 in
           let as_from2  = map fst . filter (\(_,hx) -> not (S.member hx has)) $ zip as2 has2 in

           let asgnVars1 = (nub . map (\(Is _ (Var _ x) _) -> x)) as1 in
           let asgnVars2 = (nub . map (\(Is _ (Var _ x) _) -> x)) as2 in
           let asgnVars  = nub (asgnVars1 ++ asgnVars2) in

           -- if the set of assigned variables is different in the rules, do not merge them
           if S.fromList asgnVars1 /= S.fromList asgnVars2 then
               [r1,r2]

           -- if merging does not make sense, leave the rules as they are
           -- TODO we actually need a 'complexity estimate' here
           else if (length cs_common + length cs_from1 + length cs_from2) > (length cs1 + length cs2) then
               [r1,r2]

           -- if the assignments are the same, there is no non-determinism, and we can use ground OR
           else if (length as_from1 == 0) && (length as_from2 == 0) then

               let rHead = r1 ^. ruleHead in
               let rTail = foldr1 eAnd $ ps ++ cs_common ++ [eOr (foldr eAnd eTrue cs_from1) (foldr eAnd eTrue cs_from2)] ++ as_common in
               [Rule rHead rTail]

           -- if the assignments are different, then we are dealing with non-determinism
           else

               -- label the variables in different assignments differently
               let subst1 = substFromMap $ M.fromList $ map (\x -> (x, var (x ++ "_1"))) asgnVars in
               let subst2 = substFromMap $ M.fromList $ map (\x -> (x, var (x ++ "_2"))) asgnVars in

               let cs1' = foldr eAnd eTrue $ map (applyToExpr subst1) cs_from1 in
               let cs2' = foldr eAnd eTrue $ map (applyToExpr subst2) cs_from2 in

               let as1' = map (applyToExpr subst1) as1 in
               let as2' = map (applyToExpr subst2) as2 in

               -- TODO for better efficiency, use a different function if cs1' and cs2' are mutually exclusive
               -- this could be checked e.g. using smt-solver, but we would then need an IO here

               -- TODO it can be better to put all vars into a single choose
               -- we need to link them anyway when converting to prolog
               let cats = map (\x -> eIs (var x) $ eChoose (eList [getValue subst1 x, getValue subst2 x]) (eList [cs1', cs2'])) asgnVars in

               let rHead = r1 ^. ruleHead in
               let rTail = foldr1 eAnd $ ps ++ cs_common ++ as1' ++ as2' ++ cats in
               --trace (show (pretty (r1 ^. ruleTail))) $
               --trace (show (pretty (r2 ^. ruleTail))) $
               --trace (show asgnVars) $
               --trace (show (pretty rTail)) $

               -- apply 'simplify' to get rid of intermediate variables
               [simplifyRule $ Rule rHead rTail]

foldChoose :: Expr -> Expr
foldChoose (Is ann x y) =
    Is ann x (foldChoose y)
foldChoose (Choose ann (Expr.List _ xs) (Expr.List _ bs)) =
    let ys = map foldChoose xs in
    let xs' = concat $ zipWith (\b y -> case y of {(Choose _ (Expr.List _ xs0) (Expr.List _ bs0)) -> xs0              ; _ -> [y]}) bs ys in
    let bs' = concat $ zipWith (\b y -> case y of {(Choose _ (Expr.List _ xs0) (Expr.List _ bs0)) -> map (eAnd b) bs0 ; _ -> [b]}) bs ys in
    (Choose ann (eList xs') (eList bs'))
foldChoose e = e
    --trace (show e) $ e

