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
import Control.Monad

import Annotation
import Rule
import Expr
import Simplify
import Solve
import Substitution
import qualified DatalogProgram as DP

-- | Generates all possible ground rules for `n` iterations by inlining
-- predicates with matching rules. Each predicate gets inlined once per
-- iteration.
deriveAllGroundRules :: Int -> DP.DatalogProgram -> IO DP.DatalogProgram
deriveAllGroundRules n program = program' & DP.dpRules %%~ f
  where
    -- Input program but db clauses are converted to rules
    program' = program
    f :: [Rule] -> IO [Rule]
    f rs = foldM pipeline rs [1..n]
    pipeline :: [Rule] -> Int -> IO [Rule]
    pipeline rs _ = 
      do
        let pl0 = removeDuplicateFacts rs
            pl1 = removeFalseFacts pl0
            pl2 = pl1 & traversed . ruleTail %~ simplifyAnds
        pl3 <- filterM checkConsistency pl2
        let pl4 = refreshRule "X_" <$> pl3
        return $ inlineOnce pl4

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
checkConsistency :: Rule -> IO Bool
checkConsistency r = Solve.checkSat $ r ^. ruleTail . to andsToList

-- | Removes facts that always evaluate to False
removeFalseFacts :: [Rule] -> [Rule]
removeFalseFacts = filter (\x -> x ^. ruleTail /= constBool False)

-- | Removes duplicates of facts that appear more than once
removeDuplicateFacts :: [Rule] -> [Rule]
removeDuplicateFacts = nub

