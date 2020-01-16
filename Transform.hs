module Transform where

---------------------------------------------------------
---- Transformation of a Datalog script
----  to intermediate representation
---------------------------------------------------------

import Data.Hashable
import Data.List
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S

import Aexpr
import ErrorMsg
import Parser
import Rule

-- use the rule to generate all possible facts from the given existing facts
-- p :- q1 ... qn
processRule :: (M.Map PName PMap) -> Rule -> [([Arg], Arg)]
processRule factMap (Rule as bs) =

    -- in the end, we get a list of possible thetas from which we can construct the new facts p
    let initialThetas = [(M.empty, AConstNum 1)] in
    let finalThetas   = foldl (\thetas b -> processRulePremise factMap thetas b) initialThetas bs in

    --apply obtained substitutions to args
    --accumulate the computations on private data
    -- TODO continue
    map (\ (theta,constr) ->
                         let newArgs = map (updateAexprVars theta AVar) as in
                         (newArgs,constr)
    ) finalThetas

-- assume that we already have some possible thetas evaluated from the previous qi
-- we now extend each of these thetas, possibly generating more branches
-- in addition to thetas, we store the conditions that will be verified later, all at once
processRulePremise :: (M.Map PName PMap) -> [(M.Map Var Arg, Arg)] -> RHS -> [(M.Map Var Arg, Arg)]
processRulePremise factMap thetas (Fact pName argsB) =
    if not (M.member pName factMap) then []
    else concat $ map (processFactPremise (factMap M.! pName) argsB) thetas

processRulePremise _ thetas (ABB aexpr) =
    concat $ map (processABBPremise aexpr) thetas


processABBPremise :: Arg -> (M.Map Var Arg, Arg) -> [(M.Map Var Arg, Arg)]
processABBPremise aexpr' (theta,constr) =

    let aexpr    = updateAexprVars theta AVar aexpr' in
    let allTypes = getAllAExprVarData (\x -> case x of {Free _ -> 0; _ -> 1}) aexpr in

    -- TODO we can optimize the boolean formula and see if it is computable without private variables
    if S.member 0 allTypes then error (error_nonGroundTerm aexpr) else
    let isConst = (S.size allTypes == 0) in

    if isConst then
        --if all arguments are constants, we can evaluate them immediately
        let AConstNum val = evalAexpr aexpr in
        --TODO we can do better type check here, e.g. give an error if the answer is non-boolean
        if val > 0 then [(theta,constr)] else []
    else
        --otherwise, we delegate computation to SecreC
        [(theta, ABinary AAnd constr aexpr)]

processFactPremise :: PMap -> [Arg] -> (M.Map Var Arg, Arg) -> [(M.Map Var Arg, Arg)]
processFactPremise factPredMap argsB (theta,constr) =
    concat $ map (processFactPremiseInstance factPredMap theta constr argsB) (M.keys factPredMap)

-- try to do pattern matching against all possible instance facts for "pred"
-- each matching will result in its own new theta'
-- TODO assign different indices to a LP level predicate if query it multiple times
processFactPremiseInstance :: PMap -> (M.Map Var Arg) -> Arg -> [Arg] -> [Arg] -> [(M.Map Var Arg, Arg)]
processFactPremiseInstance factPredMap theta constr argsB argsF =

    -- load the conditions needed to satisfy the fact predicate with argsF
    let constrF = factPredMap M.! argsF in

    --here theta starts branching into several new thetas
    let (unifiable,newTheta) = updateTheta theta True argsB argsF in
    if not unifiable then []
    else [(newTheta,ABinary AAnd constr constrF)]


updateTheta :: (M.Map Var Arg) -> Bool -> [Arg] -> [Arg] -> (Bool, M.Map Var Arg)
updateTheta theta unifiable [] [] = (unifiable,theta)
updateTheta theta unifiable (argB':argsB) (argF:argsF) =

    --apply theta
    let argB = case argB' of {AVar x -> if M.member x theta then theta M.! x else argB'; _ -> argB'} in

    let (theta',unifiable') = if (argB == argF) then (theta,unifiable)
                              else case argB of
                                   AVar (Free x) -> (M.insert (Free x) argF theta, unifiable)
                                   _             -> case argF of
                                                        AVar (Free x) -> (theta,unifiable)
                                                        _             -> (theta,False)
    in updateTheta theta' unifiable' argsB argsF


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
    let newFacts = M.fromList $ (map (\(x,y) -> (x, simplifyBool y))) $ concat $ map (processRule facts) (rules M.! p) in
    let factsp   = M.unionWith (\x y -> simplifyBool $ ABinary AOr x y) (if M.member p facts then facts M.! p else M.empty) newFacts in
    let facts' = M.insert p factsp facts in
    applyRule facts' rules ps

test fileName iterations = do

  (database,rules) <- parseDatalogFromFile fileName
  putStrLn (show database)
  putStrLn (show rules)
  putStrLn "--------------------------------------------------------"
  let facts = runIteration database rules 0 iterations

  let res = map (\p ->
                     "==== [[ " ++ show p ++ "]] ==== \n"
                     ++ intercalate "\n" (map (\key -> show key ++ "\t" ++ show ((facts M.! p) M.! key)) (M.keys (facts M.! p)))
                     ++ "\n"
                ) (M.keys facts)
  putStrLn $ intercalate "\n" res

