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
import Substitution

-- generate all possible ground rules for n iterations
deriveAllGroundRules :: (M.Map PName PMap) -> (M.Map PName [Rule]) -> Int -> (M.Map PName PMap)
deriveAllGroundRules facts rules n = runIteration facts rules 0 n

-- generate all possible ground rules for a single iteration
runIteration :: (M.Map PName PMap) -> (M.Map PName [Rule]) -> Int -> Int -> (M.Map PName PMap)
runIteration facts _ _ 0 = facts
runIteration facts rules prevHash n =

   -- generate rules for the next iteration
   let facts' = applyRule facts rules (M.keys rules) in

   -- stop if no more rules can be generated anymore
   -- TODO if hashes are equal, we still need to do term comparison as well, since hashing is not perfect
   let newHash = hash (show facts') in
   if (newHash == prevHash) then facts'
   else runIteration facts' rules newHash (n-1)

-- for each predicate p, generate new ground rules and merge them with existing ones
-- TODO we should do more simplification and optimization here
applyRule :: (M.Map PName PMap) -> (M.Map PName [Rule]) -> [PName] -> (M.Map PName PMap)
applyRule facts _ [] = facts
applyRule facts rules (p:ps) =
    let newFacts' = concat $ map (processRule p facts) (rules M.! p) in
    let newFacts  = M.fromListWith (\x y -> simplifyBool $ ABinary AOr x y) $ newFacts' in
    let factsp    = M.unionWith (\x y -> simplifyBool $ ABinary AOr x y) (if M.member p facts then facts M.! p else M.empty) newFacts in
    let facts' = M.insert p factsp facts in
    applyRule facts' rules ps

-- use a rule to generate new ground rules from the existing ground rules
-- p(a1,..,am) :- b1 ... bn
processRule :: PName -> M.Map PName PMap -> Rule -> [([Term], Term)]
processRule p groundRuleMap (Rule as bs) =

    -- derive a list of possible new ground rules for the fact p
    -- TODO we can remove the counter if we implement Subst using a state monad
    let newGroundRules = foldl (\th b -> processRulePremise groundRuleMap th b) [(emptyTheta, AConstBool True, 0)] bs in

    -- For each generated ground rule, apply the substitution to rule head p as well, getting newArgs
    map (\ (theta,newBody,_) ->
                         --trace ("---- " ++ p ++ " ----\n") $
                         --trace (printSubst theta) $
                         let newArgs = map (applyTheta theta) as in
                         --trace (show newArgs ++ "\n\n" ++ show (simplifyBool constr)) $
                         (newArgs,newBody)
    ) newGroundRules

-- assume that we already have some possible solutions that make previous b1...bi-1 true
-- we now extend each of these solutions to a solution for bi, possibly generating even more branches
processRulePremise :: (M.Map PName PMap) -> [(Subst, Term, Int)] -> Formula -> [(Subst, Term, Int)]
processRulePremise groundRuleMap thetas b =

    case b of
        -- if b is a fact, we take all existing solutions that make this fact true
        Fact pName argsB ->
            if not (M.member pName groundRuleMap) then []
            else concat $ map (processFactPremise (groundRuleMap M.! pName) argsB) thetas

        -- if is an arithmetic-black-box expression, we try to evaluate it if we can, or postpone the evaluation
        ABB aexpr ->
            concat $ map (processABBPremise aexpr) thetas


processABBPremise :: Term -> (Subst, Term, Int) -> [(Subst, Term, Int)]
processABBPremise aexpr' (theta',constr,cnt) =

    let aexpr''        = applyTheta theta' aexpr' in
    let (aexpr, theta) = assignmentsToTheta aexpr'' theta' in

    let allTypes = getAllAExprVarData (\x -> case x of {Free _ -> 0; _ -> 1}) aexpr in

    -- TODO we can apply constant folding already here and see if it is computable without private variables
    let isConst = (S.size allTypes == 0) in

    if isConst then

        --if all arguments are constants, we can evaluate them immediately
        let AConstBool val = evalAexpr aexpr in
        if val == True then [(theta,constr,cnt)] else []
    else
        --otherwise, we delegate computation to SecreC
        [(theta, ABinary AAnd constr aexpr,cnt)]

processFactPremise :: PMap -> [Term] -> (Subst, Term, Int) -> [(Subst, Term, Int)]
processFactPremise factPredMap argsB (theta,constr,cnt) =

    -- here theta starts branching into several new thetas, by considering different possible matchings
    concat $ map (unifyPredicate factPredMap theta constr cnt argsB) (M.keys factPredMap)

-- try to unify a predicate with a ground instance of this predicate
unifyPredicate :: PMap -> Subst -> Term -> Int -> [Term] -> [Term] -> [(Subst, Term, Int)]
unifyPredicate factPredMap thetaB' constr cnt' argsB argsF =

    -- load a fresh instance of analyzed fact with arguments argsF
    let (cnt, thetaF', constrF) = refreshVariableNames cnt' $ factPredMap M.! argsF in

    -- try to match
    let (thetaB,thetaF,unifiable,constrUnif) = unifyArgs thetaB' thetaF' True (AConstBool True) argsB argsF in

    -- we add a potential solution iff the terms are unifiable
    if not unifiable then []
    else [(thetaB, ABinary AAnd constr $ ABinary AAnd (applyTheta thetaF constrF) constrUnif, cnt)]

-- unification of arguments of two predicates (assumes that the predicate symbol is the same)
unifyArgs :: Subst -> Subst -> Bool -> Term -> [Term] -> [Term] -> (Subst, Subst, Bool, Term)
unifyArgs thetaB thetaF unifiable constr [] [] = (thetaB,thetaF,unifiable,constr)
unifyArgs thetaB thetaF unifiable constr (argB':argsB') (argF':argsF') =

    -- apply substitutions that we already have
    let argB = applyTheta thetaB argB' in
    let argF = applyTheta thetaF argF' in

    let (thetaB', thetaF', unifiable', constr') =

          -- if the terms already unify, do not add any more constraints
          if (argB == argF) then
            (thetaB,thetaF,unifiable,constr)

          -- otherwise, the terms may still unify, but we possibly need to update substitution
          else
            case (argB,argF) of

                -- if one of the matched variables is free, add its new valuation to the substitution
                (AVar x@(Free _), _) -> (updateTheta x argF thetaB, thetaF, unifiable, constr)
                (_, AVar y@(Free _)) -> (thetaB, updateTheta y argB thetaF, unifiable, constr)

                -- different constants can never be unifiable
                -- TODO we need to go deeper and apply constant propagation here
                (AConstBool _, AConstBool _) -> (thetaB, thetaF, False, constr)
                (AConstNum _,  AConstNum _)  -> (thetaB, thetaF, False, constr)
                (AConstStr _,  AConstStr _)  -> (thetaB, thetaF, False, constr)

                -- if terms are potentially unifiable (depending on private data), we add an equality constraint
                _  -> (thetaB, thetaF, unifiable, ABinary AAnd constr (ABinary AEQ argB argF))

    in unifyArgs thetaB' thetaF' unifiable' constr' argsB' argsF'



--------------------------------
-- this is for debugging only
showAllRules :: (M.Map PName PMap) -> String
showAllRules rules =
  let res = map (\p ->
                     "%% [[ " ++ p ++ "]] %% \n"
                     ++ intercalate "\n\n" (map (\key -> predToString "" p key ((rules M.! p) M.! key) ++ "\n") (M.keys (rules M.! p)))
                ) (M.keys rules)
  in
  intercalate "\n" res


