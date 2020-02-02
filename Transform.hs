module Transform where

---------------------------------------------------------
---- Transformation of a Datalog script
----  to intermediate representation
---------------------------------------------------------

import Control.Monad
import Data.Hashable
import Data.List
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S

import Aexpr
import ErrorMsg
import ProgramOptions
import Rule

-- a prefix of all fresh variables
nv = "_X"

-- this is for debugging only
printSubst theta =
    concat $ map (\(k,v) -> show k ++ " -> " ++ show v ++ "\n") (M.toList theta)

-- update the substitution
updateTheta :: Var -> Arg -> M.Map Var Arg -> M.Map Var Arg
updateTheta a b theta =

    let theta' = M.insert a b theta in
    M.fromList $ map (\(x,y) -> case y of
                                    AVar z -> if z == a then (x,b) else (x,y)
                                    _      -> (x,y)
                     ) (M.toList theta')

-- use the rule to generate all possible facts from the given existing facts
-- p :- q1 ... qn
processRule :: PName -> (M.Map PName PMap) -> Rule -> [([Arg], Arg)]
processRule pname factMap (Rule as bs) =

    -- in the end, we get a list of possible thetas from which we can construct the new facts p
    let initialThetas = [(M.empty, AConstBool True, 0)] in
    let finalThetas   = foldl (\thetas b -> processRulePremise factMap thetas b) initialThetas bs in

    --apply obtained substitutions to args
    --accumulate the computations on private data
    map (\ (theta,constr,_) ->
                         --trace ("---- " ++ pname ++ " ----\n") $
                         --trace (printSubst theta) $
                         let newArgs = map (updateAexprVars theta AVar) as in
                         --trace (show newArgs ++ "\n\n" ++ show (simplifyBool constr)) $
                         (newArgs,constr)
    ) finalThetas

-- assume that we already have some possible thetas evaluated from the previous qi
-- we now extend each of these thetas, possibly generating more branches
-- in addition to thetas, we store the conditions that will be verified later, all at once
processRulePremise :: (M.Map PName PMap) -> [(M.Map Var Arg, Arg, Int)] -> RHS -> [(M.Map Var Arg, Arg, Int)]
processRulePremise factMap thetas (Fact pName argsB) =
    if not (M.member pName factMap) then []
    else concat $ map (processFactPremise (factMap M.! pName) argsB) thetas

processRulePremise _ thetas (ABB aexpr) =
    concat $ map (processABBPremise aexpr) thetas


processABBPremise :: Arg -> (M.Map Var Arg, Arg, Int) -> [(M.Map Var Arg, Arg, Int)]
processABBPremise aexpr' (theta,constr,cnt) =

    let aexpr    = updateAexprVars theta AVar aexpr' in
    let allTypes = getAllAExprVarData (\x -> case x of {Free _ -> 0; _ -> 1}) aexpr in

    -- TODO we can apply constant folding and see if it is computable without private variables
    let isConst = (S.size allTypes == 0) in

    if isConst then
        --if all arguments are constants, we can evaluate them immediately
        let AConstBool val = evalAexpr aexpr in
        --TODO we can do better type check here, e.g. give an error if the answer is non-boolean
        if val == True then [(theta,constr,cnt)] else []
    else
        --otherwise, we delegate computation to SecreC
        [(theta, ABinary AAnd constr aexpr,cnt)]

processFactPremise :: PMap -> [Arg] -> (M.Map Var Arg, Arg, Int) -> [(M.Map Var Arg, Arg, Int)]
processFactPremise factPredMap argsB (theta,constr,cnt) =
    concat $ map (processFactPremiseInstance factPredMap theta constr cnt argsB) (M.keys factPredMap)

-- try to do pattern matching against all possible instance facts for "pred"
-- each matching will result in its own new theta'
-- TODO assign different indices to a LP level predicate if query it multiple times
processFactPremiseInstance :: PMap -> (M.Map Var Arg) -> Arg -> Int -> [Arg] -> [Arg] -> [(M.Map Var Arg, Arg, Int)]
processFactPremiseInstance factPredMap thetaB' constr cnt' argsB argsF =

    -- load the conditions needed to satisfy the fact predicate with argsF
    let f x c = case x of
                  Bound domain vt z -> AVar (Bound domain vt (nv ++ show c))
                  Free z       -> AVar (Free (nv ++ show c))
    in
    let argVarMap = M.fromList $ map (\z@(AVar x) -> (x,z)) (filter (\x -> case x of {AVar _ -> True; _ -> False}) argsF) in
    let (cnt, thetaF', constrF) = updateAexprVarsFold f cnt' M.empty $ factPredMap M.! argsF in

    --here theta starts branching into several new thetas
    let (thetaB,thetaF,unifiable,newCnt) = processArgs cnt thetaB' thetaF' True argsB argsF in

    if not unifiable then []
    else [(thetaB, ABinary AAnd constr (updateAexprVars thetaF AVar constrF), newCnt)]

processArgs :: Int -> (M.Map Var Arg) -> (M.Map Var Arg) -> Bool -> [Arg] -> [Arg] -> (M.Map Var Arg, M.Map Var Arg, Bool, Int)
processArgs cnt thetaB thetaF unifiable [] [] = (thetaB,thetaF,unifiable,cnt)
processArgs cnt thetaB thetaF unifiable (argB':argsB) (argF':argsF) =

    --apply theta
    let argB = case argB' of {AVar x -> if M.member x thetaB then thetaB M.! x else argB'; _ -> argB'} in
    let argF = case argF' of {AVar x -> if M.member x thetaF then thetaF M.! x else argF'; _ -> argF'} in

    -- TODO argF can also be a more complex expression that we may want to rewrite
    let (thetaB', thetaF', unifiable', cnt') = if (argB == argF) then (thetaB,thetaF,unifiable,cnt) else
            case (argB,argF) of

                (AVar (Free x), _                  ) -> (updateTheta (Free x) argF thetaB, thetaF, unifiable, cnt)

                (_            , AVar (Free y)      ) -> (thetaB, updateTheta (Free y      ) argB thetaF, unifiable, cnt)
                (_            , AVar (Bound domain ty y)) -> (thetaB, updateTheta (Bound domain ty y) argB thetaF, unifiable, cnt)

                _                                    -> (thetaB, thetaF, False, cnt)

    in processArgs cnt' thetaB' thetaF' unifiable' argsB argsF


runIteration :: (M.Map PName PMap) -> (M.Map PName [Rule]) -> Int -> Int -> (M.Map PName PMap)
runIteration facts _ _ 0 = facts
runIteration facts rules prevHash n =

   -- generate facts for the next iteration
   let facts' = applyRule facts rules (M.keys rules) in

   --stop if no more rules can be generated anymore
   let newHash = hash (show facts') in
   if (newHash == prevHash) then facts'
   else runIteration facts' rules newHash (n-1)

applyRule :: (M.Map PName PMap) -> (M.Map PName [Rule]) -> [PName] -> (M.Map PName PMap)
applyRule facts _ [] = facts
applyRule facts rules (p:ps) =
    let newFacts' = concat $ map (processRule p facts) (rules M.! p) in
    let newFacts  = M.fromListWith (\x y -> simplifyBool $ ABinary AOr x y) $ newFacts' in
    let factsp    = M.unionWith (\x y -> simplifyBool $ ABinary AOr x y) (if M.member p facts then facts M.! p else M.empty) newFacts in
    let facts' = M.insert p factsp facts in
    applyRule facts' rules ps

showResult :: (M.Map PName PMap) -> String
showResult facts =
  let res = map (\p ->
                     "==== [[ " ++ show p ++ "]] ==== \n"
                     ++ intercalate "\n\n" (map (\key -> predToString "" p key ((facts M.! p) M.! key) ++ "\n") (M.keys (facts M.! p)))
                ) (M.keys facts)
  in
  intercalate "\n" res
