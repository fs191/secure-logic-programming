module Transform
  ( deriveAllGroundRules
  ) where

---------------------------------------------------------
---- Transformation of a Datalog script
----  to intermediate representation
---------------------------------------------------------

import Data.Hashable
import qualified Data.Map as M

import Aexpr
import Rule
import Substitution
import qualified DatalogProgram as DP
import DBClause

type PMap = M.Map [Term] Formula

-- generate all possible ground rules for n iterations
deriveAllGroundRules :: DP.PPDatalogProgram -> Int -> DP.PPDatalogProgram
deriveAllGroundRules program n = DP.setRules res program
  where f = toPMapMap $ DP.facts program
        r = toMap $ DP.rules program
        res = fromPMapMap $ runIteration f r 0 n

-- generate all possible ground rules for a single iteration
runIteration :: (M.Map String PMap) -> (M.Map String [Rule]) -> Int -> Int -> (M.Map String PMap)
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
applyRule :: (M.Map String PMap) -> (M.Map String [Rule]) -> [String] -> (M.Map String PMap)
applyRule facts _ [] = facts
applyRule facts rules (p:ps) =
    let newFacts' = concat $ map (processRule p facts) (rules M.! p) in
    let newFacts  = M.fromListWith (\x y -> simplifyBool $ BBinary BOr x y) $ newFacts' in
    let factsp    = M.unionWith (\x y -> simplifyBool $ BBinary BOr x y) (if M.member p facts then facts M.! p else M.empty) newFacts in
    let facts' = M.insert p factsp facts in
    applyRule facts' rules ps

-- use a rule to generate new ground rules from the existing ground rules
-- p(a1,..,am) :- b1 ... bn
processRule :: String -> M.Map String PMap -> Rule -> [([Term], Formula)]
processRule _ groundRuleMap r =

    -- derive a list of possible new ground rules for the fact p
    -- TODO we can remove the counter if we implement Subst using a state monad
    let bexpr = premise r
        as = args r
        newGroundRules = processRulePremise groundRuleMap [(emptyTheta, BConstBool True, 0)] bexpr in

    -- For each generated ground rule, apply the substitution to rule head p as well, getting newArgs
    map (\ (theta,newBody,_) ->
                         --trace ("---- " ++ p ++ " ----\n") $
                         --trace (printSubst theta) $
                         let newArgs = map (applyToTerm theta) as in
                         --trace (show newArgs ++ "\n\n" ++ show (simplifyBool constr)) $
                         (newArgs,newBody)
    ) newGroundRules

-- assume that we already have some possible solutions that make previous b1...bi-1 true
-- we now extend each of these solutions to a solution for bi, possibly generating even more branches
processRulePremise :: (M.Map String PMap) -> [(Subst, Formula, Int)] -> Formula -> [(Subst, Formula, Int)]
processRulePremise groundRuleMap thetas bexpr = concat $ map (processFormula groundRuleMap bexpr) thetas


processFormula :: (M.Map String PMap) -> Formula -> (Subst, Formula, Int) -> [(Subst, Formula, Int)]
processFormula groundRuleMap bexpr' (theta', constr, cnt) =

    let bexpr''        = applyToFormula theta' bexpr' in
    let (bexpr, theta) = assignmentsToTheta bexpr'' theta' in
    let solution       = (theta, constr, cnt) in

    case bexpr of

        -- True does not modify the solution
        BConstBool True   -> [solution]
        -- False nullifies the solution
        BConstBool False  -> []

        -- satisfiability of an (in)equality predicate is added to solution constraints
        BBinPred  _ _ _   -> [(theta, BBinary BAnd constr bexpr,cnt)]

        -- TODO we can only apply negation to terms without predicate calls for now
        BUnary BNot x     ->  [(theta, BBinary BAnd constr (BUnary BNot bexpr),cnt)]

        BBinary BOr x1 x2  -> let solutions1 = processFormula groundRuleMap x1 solution in
                              let solutions2 = processFormula groundRuleMap x2 solution in
                              solutions1 ++ solutions2

        BBinary BAnd x1 x2 -> let solutions1 = processFormula groundRuleMap x1 solution in
                              let solutions2 = concat $ map (processFormula groundRuleMap x2) solutions1 in
                              solutions2

        -- n-ary And and Or are analogous to binary
        BNary BOrs xs      -> concat $ map (\x -> processFormula groundRuleMap x solution) xs
        BNary BAnds xs     -> foldr (\x solutions -> concat $ map (processFormula groundRuleMap x) solutions) [solution] xs

        -- if b is a fact, we take all existing solutions that make this fact true
        BListPred (BPredName pName) argsB ->
            if not (M.member pName groundRuleMap) then []
            else
               let factPredMap = groundRuleMap M.! pName in
               concat $ map (unifyPredicate factPredMap theta constr cnt argsB) (M.keys factPredMap)


-- try to unify a predicate with a ground instance of this predicate
unifyPredicate :: PMap -> Subst -> Formula -> Int -> [Term] -> [Term] -> [(Subst, Formula, Int)]
unifyPredicate factPredMap thetaB' constr cnt' argsB argsF =

    -- load a fresh instance of analyzed fact with arguments argsF
    let (cnt, thetaF', constrF) = refreshVarNames cnt' $ factPredMap M.! argsF in

    -- try to match
    let (thetaB,thetaF,unifiable,constrUnif) = unifyArgs thetaB' thetaF' True (BConstBool True) argsB argsF in

    -- we add a potential solution iff the terms are unifiable
    if not unifiable then []
    else [(thetaB, BBinary BAnd constr $ BBinary BAnd (applyToFormula thetaF constrF) constrUnif, cnt)]

-- unification of arguments of two predicates (assumes that the predicate symbol is the same)
unifyArgs :: Subst -> Subst -> Bool -> Formula -> [Term] -> [Term] -> (Subst, Subst, Bool, Formula)
unifyArgs thetaB thetaF unifiable constr [] [] = (thetaB,thetaF,unifiable,constr)
unifyArgs thetaB thetaF unifiable constr (argB':argsB') (argF':argsF') =

    -- apply substitutions that we already have
    let argB = applyToTerm thetaB argB' in
    let argF = applyToTerm thetaF argF' in

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
                (AConstNum _,  AConstNum _)  -> (thetaB, thetaF, False, constr)
                (AConstStr _,  AConstStr _)  -> (thetaB, thetaF, False, constr)

                -- if terms are potentially unifiable (depending on private data), we add an equality constraint
                _  -> (thetaB, thetaF, unifiable, BBinary BAnd constr (BBinPred BEQ argB argF))

    in unifyArgs thetaB' thetaF' unifiable' constr' argsB' argsF'


