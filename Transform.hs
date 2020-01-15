module Transform where

---------------------------------------------------------
---- This module transforms a Datalog script
----  to a SecreC application
---------------------------------------------------------

import Data.Hashable
import Data.List
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S

import Aexpr

-- names of attributes, free variables, and predicates are strings
type AName = String
type VName = String
type PName = String

-- predicate argumnet
data Arg a = Public AName | Private AName | ConstNum a | ConstStr String | Var VName deriving (Ord,Eq,Show)

-- rule
data Rule a = Rule [Arg a] [RHS a] deriving Show

-- a RHS predicate can be either arithetic blackbox operation (like "=") or a fact defined in LP
data RHS  a = Fact PName [Arg a] | ABB (AExpr (Arg a)) deriving Show

-- use the rule to generate all possible facts from the given existing facts
-- p :- q1 ... qn
processRule :: (Ord a, Show a) => (Arg a -> Int) -> (M.Map PName (M.Map [Arg a] (AExpr (Arg a)))) -> Rule a -> [([Arg a], AExpr (Arg a))]
processRule f factMap (Rule as bs) =

    -- in the end, we get a list of possible thetas from which we can construct the new facts p
    let initialThetas = [(M.empty, AConstNum 1)] in
    let finalThetas   = foldl (\thetas b -> processRulePremise f factMap thetas b) initialThetas bs in

    --apply obtained substitutions to args
    --accumulate the computations on private data
    -- TODO continue
    map (\ (theta,constr) ->
                         let newArgs = map (\x -> if M.member x theta then theta M.! x else x) as in
                         (newArgs,constr)
    ) finalThetas

-- assume that we already have some possible thetas evaluated from the previous qi
-- we now extend each of these thetas, possibly generating more branches
-- in addition to thetas, we store the conditions that will be verified later, all at once
processRulePremise :: (Ord a, Show a) => (Arg a -> Int) -> (M.Map PName (M.Map [Arg a] (AExpr (Arg a)))) -> [(M.Map (Arg a) (Arg a), AExpr (Arg a))] -> RHS a -> [(M.Map (Arg a) (Arg a), AExpr (Arg a))]
processRulePremise _ factMap thetas (Fact pName argsB) =
    if not (M.member pName factMap) then []
    else concat $ map (processFactPremise (factMap M.! pName) argsB) thetas

processRulePremise f _ thetas (ABB aexpr) =
    concat $ map (processABBPremise f aexpr) thetas


processABBPremise :: (Ord a, Show a) => (Arg a -> Int) -> AExpr (Arg a) -> (M.Map (Arg a) (Arg a), AExpr (Arg a)) -> [(M.Map (Arg a) (Arg a), AExpr (Arg a))]
processABBPremise f aexpr' (theta,constr) =

    let aexpr    = updateAexprVars theta id aexpr' in
    let allTypes = getAllAExprVarData (\x -> case x of {Var _ -> 0; ConstNum _ -> 1; ConstStr _ -> 1; _ -> 2}) aexpr in

    -- TODO we can optimize the boolean formula and see if it is computable without private variables
    if S.member 0 allTypes then error ("ERROR! unbound variable in an ABB call " ++ show aexpr) else
    let isConst = foldr (&&) True (map (\x -> case x of {1 -> True; _ -> False}) (S.toList allTypes)) in

    if isConst then
        --if all arguments are constants, we can evaluate them immediately
        let AConstNum val = evalAexpr f aexpr in
        --TODO we can do better type check here, e.g. give an error if the answer is non-boolean
        if val > 0 then [(theta,constr)] else []
    else
        --otherwise, we delegate computation to SecreC
        [(theta, ABinary AAnd constr aexpr)]

processFactPremise :: (Ord a, Show a) => (M.Map [Arg a] (AExpr (Arg a))) -> [Arg a] -> (M.Map (Arg a) (Arg a), AExpr (Arg a)) -> [(M.Map (Arg a) (Arg a), AExpr (Arg a))]
processFactPremise factPredMap argsB (theta,constr) =
    concat $ map (processFactPremiseInstance factPredMap theta constr argsB) (M.keys factPredMap)

-- try to do pattern matching against all possible instance facts for "pred"
-- each matching will result in its own new theta'
-- TODO assign different indices to a LP level predicate if query it multiple times
processFactPremiseInstance :: (Ord a, Show a) => (M.Map [Arg a] (AExpr (Arg a))) -> (M.Map (Arg a) (Arg a)) -> AExpr (Arg a) -> [Arg a] -> [Arg a] -> [(M.Map (Arg a) (Arg a), AExpr (Arg a))]
processFactPremiseInstance factPredMap theta constr argsB argsF =

    -- load the conditions needed to satisfy the fact predicate with argsF
    let constrF = factPredMap M.! argsF in

    --here theta starts branching into several new thetas
    let (unifiable,newTheta) = updateTheta theta True argsB argsF in
    if not unifiable then []
    else [(newTheta,ABinary AAnd constr constrF)]


updateTheta :: (Ord a, Show a) => (M.Map (Arg a) (Arg a)) -> Bool -> [Arg a] -> [Arg a] -> (Bool, M.Map (Arg a) (Arg a))
updateTheta theta unifiable [] [] = (unifiable,theta)
updateTheta theta unifiable (argB':argsB) (argF:argsF) =

    --apply theta
    let argB = if M.member argB' theta then theta M.! argB' else argB' in
    let (theta',unifiable') = if (argB == argF) then (theta,unifiable)
                              else case argB of
                                   Var x -> (M.insert argB argF theta, unifiable)
                                   _     -> case argF of
                                                Var x -> (theta,unifiable)
                                                _     -> (theta,False)
    in updateTheta theta' unifiable' argsB argsF



test iterations = do

  -- TODO need to parse these things from file

  --simulate a database snapshot
  let database = M.fromList [("eds", M.fromList [([Public "name", Public "department", Private "salary"], AConstNum 1)]),
                             ( "dm", M.fromList [([Public "department", Public "manager"],                AConstNum 1)])]
                 :: (M.Map PName (M.Map [Arg Int] (AExpr (Arg Int))))


  -- generate the rules
  let rules = M.fromList [("secureEDS", [Rule [Var "E", Var "D", Var "S"]    [Fact "eds" [Var "E", Var "D", Var "S"], ABB (ABinary ALT (AVar (Var "S")) (AConstNum 100000))],
                                         Rule [Var "E", Var "D", ConstNum 0] [Fact "eds" [Var "E", Var "D", Var "S"], ABB (ABinary ALE (AConstNum 100000) (AVar (Var "S")))]]),
                          ("viewESM",   [Rule [Var "E", Var "S", Var "M"]    [Fact "secureEDS" [Var "E", Var "D", Var "S"], Fact "dm" [Var "D", Var "M"] ]])]
              :: (M.Map PName [Rule Int])

  let f = (\x -> case x of {ConstNum x -> x; _ -> error $ "ERROR! non-integer variables in " ++ show x})
  let facts = runIteration f database rules 0 iterations

  putStrLn "--------------------------------------------------------"
  let res = map (\p ->
                     "==== [[ " ++ show p ++ "]] ==== \n"
                     ++ intercalate "\n" (map (\key -> show key ++ "\t" ++ show ((facts M.! p) M.! key)) (M.keys (facts M.! p)))
                     ++ "\n"
                ) (M.keys facts)
  putStrLn $ intercalate "\n" res


runIteration :: (Ord a, Show a) => (Arg a -> Int) -> (M.Map PName (M.Map [Arg a] (AExpr (Arg a)))) -> (M.Map PName [Rule a]) -> Int -> Int -> (M.Map PName (M.Map [Arg a] (AExpr (Arg a))))
runIteration _ facts _ _ 0 = facts
runIteration f facts rules prevHash n =

   -- generate facts for the next iteration
   let facts' = applyRule f facts rules (M.keys rules) in

   --stop if no more rules can be generated anymore
   let newHash = hash (show facts') in
   if (newHash == prevHash) then facts'
   else runIteration f facts' rules newHash (n-1)

applyRule :: (Ord a, Show a) => (Arg a -> Int) -> (M.Map PName (M.Map [Arg a] (AExpr (Arg a)))) -> (M.Map PName [Rule a]) -> [PName] -> (M.Map PName (M.Map [Arg a] (AExpr (Arg a))))
applyRule _ facts _ [] = facts
applyRule f facts rules (p:ps) =
    let newFacts = M.fromList $ (map (\(x,y) -> (x, simplifyBool y))) $ concat $ map (processRule f facts) (rules M.! p) in
    let factsp   = M.unionWith (\x y -> simplifyBool $ ABinary AOr x y) (if M.member p facts then facts M.! p else M.empty) newFacts in
    let facts' = M.insert p factsp facts in
    applyRule f facts' rules ps

