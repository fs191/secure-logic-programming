{-# LANGUAGE ScopedTypeVariables #-}

module Transform
  ( deriveAllGroundRules
  ) where

---------------------------------------------------------
---- Transformation of a Datalog script
----   to intermediate representation
---------------------------------------------------------

import Data.Generics.Uniplate.Operations as U
import qualified Data.List.Safe as S
import Data.Maybe

import Control.Lens as L
import Control.Monad

import Rule
import Expr
import Solve
import Substitution
import DatalogProgram

-- | Generates all possible ground rules for `n` iterations by inlining
-- predicates with matching rules. Each predicate gets inlined once per
-- iteration.
deriveAllGroundRules :: Int -> DatalogProgram -> IO DatalogProgram
deriveAllGroundRules n program = f program'
  where
    -- Input program but db clauses are converted to rules
    program' = program
    f :: DatalogProgram -> IO DatalogProgram 
    f rs = foldM pipeline rs [1..n]
    pipeline :: DatalogProgram -> Int -> IO DatalogProgram
    pipeline dp i = 
      do
        pl <- dp & id %~ (inlineOnce (n-i+1))
                 & dpRules . traversed %~ refreshRule "X_"
                 & dpRules %%~ filterM checkConsistency
        return $ pl & dpRules . traversed . ruleTail %~ simplifyAnds
                    & dpRules %~ removeFalseFacts
                    & dpRules %~ removeDuplicateFacts

-- | Tries to unify the first predicate in each rule body with an appropriate rule
inlineOnce :: Int -> DatalogProgram -> DatalogProgram
inlineOnce j dp = dp & dpRules .~ rs'
  where
    rs' = potentialSrc <> inlined
    rs  = dp ^. dpRules
    potentialSrc = filter (isGround dp) rs
    inlined = do
      tgt <- refreshRule "T_" <$> rs
      let ttl = tgt ^. ruleTail
      let fil (x,_) = isPredicate x && isIDBFact dp x
      let tlPreds = filter fil $ U.contexts ttl
      (p,mut) <- S.head tlPreds
      let res
            | null tlPreds = return tgt
            -- we can discard those rules for which there is no hope to become ground in the remaining number of steps
            -- 'nullify' performs better and its correct, but for valid automated tests we currently need to leave them
            | length tlPreds > j = return tgt -- (tgt & ruleTail %~ nullify)
            | otherwise = 
                do
                  src <- findRules dp $ p ^. predName
                  let shd = src ^. ruleHead
                  let stl = src ^. ruleTail
                  let subst = unify shd p
                  let subster x = applySubst x (tgt & ruleTail .~ mut stl)
                  maybeToList $ subster <$> subst
      res


nullify :: Expr -> Expr
nullify x = constBool False

-- Removes duplicate terms from AND operations at the root expression
simplifyAnds :: Expr -> Expr
simplifyAnds x = foldr1 eAnd . S.nub . filter (not . isAnd) $ simplifyAnds' x

simplifyAnds' :: Expr -> [Expr]
simplifyAnds' (And _ x y) = simplifyAnds' x <> simplifyAnds' y
simplifyAnds' x = [x]

isAnd :: Expr -> Bool
isAnd And{} = True
isAnd _     = False

isPredicate :: Expr -> Bool
isPredicate Pred{} = True
isPredicate _      = False

isGround :: DatalogProgram -> Rule -> Bool
isGround dp r = all (not . isIDBFact dp) . U.universe $ r ^. ruleTail

-- rewrite the assignments and look for contradictions
checkConsistency :: Rule -> IO Bool
checkConsistency r = Solve.checkSat $ r ^. ruleTail . to andsToList

-- | Removes facts that always evaluate to False
removeFalseFacts :: [Rule] -> [Rule]
removeFalseFacts = filter (\x -> x ^. ruleTail /= constBool False)

-- | Removes duplicates of facts that appear more than once
removeDuplicateFacts :: [Rule] -> [Rule]
removeDuplicateFacts = S.nub

