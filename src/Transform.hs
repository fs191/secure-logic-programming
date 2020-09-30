{-# LANGUAGE ScopedTypeVariables #-}

module Transform
  ( deriveAllGroundRules
  ) where

---------------------------------------------------------
---- Transformation of a Datalog script
----   to intermediate representation
---------------------------------------------------------

import Data.Generics.Uniplate.Operations as U
import Data.List
import Data.Maybe

import Control.Lens as L

import Annotation
import Rule
import Expr
import Simplify
--import Solve
import Substitution
import qualified DatalogProgram as DP

-- | Generates all possible ground rules for `n` iterations by inlining
-- predicates with matching rules. Each predicate gets inlined once per
-- iteration.
deriveAllGroundRules :: Int -> DP.DatalogProgram -> DP.DatalogProgram
deriveAllGroundRules n program = program' & DP.dpRules %~ f
  where
    -- Input program but db clauses are converted to rules
    program' = program
    f :: [Rule] -> [Rule]
    f = foldl (.) id $ replicate n pipeline
    pipeline :: [Rule] -> [Rule]
    pipeline 
      = removeDuplicateFacts 
      . removeFalseFacts 
      . (traversed . ruleTail %~ simplifyAnds :: [Rule] -> [Rule])
      . (traversed %~ simplifyRule)
      . map (refreshRule "X_") 
      . inlineOnce

-- | Tries to unify each predicate in each rule body with an appropriate rule
inlineOnce :: [Rule] -> [Rule]
inlineOnce rs =
  rs <> inlined
  where
    inlined = catMaybes $ do
      tgt <- refreshRule "T_" <$> rs
      src <- rs
      let shd = src ^. ruleHead
      let stl = src ^. ruleTail
      let ttl = tgt ^. ruleTail
      (p@Pred{}, mut) <- U.contexts ttl
      let subst = unify shd p
      [flip applySubst (tgt & ruleTail .~ mut stl) <$> subst]

-- Removes duplicate terms from AND operations at the root expression
simplifyAnds :: Expr -> Expr
simplifyAnds x = foldr1 eAnd . nub . filter (not . isAnd) $ simplifyAnds' x

simplifyAnds' :: Expr -> [Expr]
simplifyAnds' (And _ x y) = simplifyAnds' x <> simplifyAnds' y
simplifyAnds' x = [x]

isAnd :: Expr -> Bool
isAnd And{} = True
isAnd _     = False

-- rewrite the assignments and look for contradictions
simplifyRule :: Rule -> Rule
simplifyRule r =

    let rName = ruleName r in
    let rBody = simplifyAnds' $ r ^. ruleTail in
    let rArgs = args r in
    let (boundedVars, freeVars) = partition (\z -> z ^. annotation ^. annBound) rArgs in
    let boundedVarNames = concat $ map varNames boundedVars in
    let freeVarNames = concat $ map varNames freeVars in
    let newRuleBody = simplify rBody (boundedVarNames, freeVarNames) in

    -- TODO run z3 if the corresponding flag is set to true
    -- we need to reside in an IO monad for this
    -- z3 should be pre-installed, check that "z3 -in" indeed runs an interactive session
    -- result <- Solve.checkSat rBody

    let newRuleTail = foldr1 eAnd . nub . filter (not . isAnd) $ newRuleBody in
    rule rName rArgs newRuleTail

-- | Removes facts that always evaluate to False
removeFalseFacts :: [Rule] -> [Rule]
removeFalseFacts = filter (\x -> x ^. ruleTail /= constBool False)

-- | Removes duplicates of facts that appear more than once
removeDuplicateFacts :: [Rule] -> [Rule]
removeDuplicateFacts = nub

