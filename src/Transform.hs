module Transform
  ( deriveAllGroundRules
  ) where

---------------------------------------------------------
---- Transformation of a Datalog script
----  to intermediate representation
---------------------------------------------------------

import Data.Generics.Uniplate.Operations as U
import Data.Maybe
import Data.List
import qualified Data.Set as S

import Control.Applicative
import Control.Lens

import Rule
import Expr
import Substitution
import DBClause
import qualified DatalogProgram as DP

-- | Generates all possible ground rules for n iterations
deriveAllGroundRules :: DP.DatalogProgram -> Int -> DP.DatalogProgram
deriveAllGroundRules program n = program & DP.ruleLens %~ f
  where
    f :: [Rule] -> [Rule]
    f x = foldl (.) id (replicate n pipeline) x
    pipeline = 
      removeDuplicateFacts .
      removeFalseFacts .
      liftA simplify . 
      (traversed . ruleTail %~ simplifyAnds) .
      (traversed . ruleTail %~ simplifyVars) . 
      inlineOnce

-- | Tries to unify each predicate in each rule body with an appropriate rule
inlineOnce :: [Rule] -> [Rule]
inlineOnce rs =
  rs <> do
    tgt <- refreshRule "_X" <$> rs
    src <- refreshRule "_Y" <$> rs
    let shd = src ^. ruleHead
    let stl = src ^. ruleTail
    let ttl = tgt ^. ruleTail
    (p@(Pred _ _), mut) <- U.contexts ttl
    let subst = unify shd p
    s <- maybeToList subst
    return . applySubst s $ tgt & ruleTail .~ mut stl

-- | Rewrites constant terms to simpler terms
simplify :: Rule -> Rule
simplify r = r & ruleTail %~ U.rewrite f
  where 
    f (Binary And (ConstBool True) x) = Just x
    f (Binary And x (ConstBool True)) = Just x
    f (Binary And (ConstBool False) _) = Just $ ConstBool False
    f (Binary And _ (ConstBool False)) = Just $ ConstBool False
    f (Binary Or (ConstBool True) _) = Just $ ConstBool True
    f (Binary Or _ (ConstBool True)) = Just $ ConstBool True
    f (Binary Or (ConstBool False) x) = Just x
    f (Binary Or x (ConstBool False)) = Just x
    f (Binary Add (ConstNum x) (ConstNum y)) = Just . ConstNum $ x + y
    f (Binary Sub (ConstNum x) (ConstNum y)) = Just . ConstNum $ x - y
    f (Binary Mult (ConstNum x) (ConstNum y)) = Just . ConstNum $ x * y
    f (Binary Min (ConstNum x) (ConstNum y)) = Just . ConstNum $ x `min` y
    f (Binary Max (ConstNum x) (ConstNum y)) = Just . ConstNum $ x `max` y
    f (Binary BLT (ConstNum x) (ConstNum y)) = Just . ConstBool $ x < y
    f (Binary BGT (ConstNum x) (ConstNum y)) = Just . ConstBool $ x > y
    f (Binary BLE (ConstNum x) (ConstNum y)) = Just . ConstBool $ x <= y
    f (Binary BGE (ConstNum x) (ConstNum y)) = Just . ConstBool $ x >= y
    f (Binary BEQ (ConstNum x) (ConstNum y)) = Just . ConstBool $ x == y
    f (Binary BEQ (Var x) (Var y))
      | x == y    = Just $ ConstBool True
      | otherwise = Nothing
    f _ = Nothing

-- | Removes any unnecessary variable equalities (e.g. X=Y)
simplifyVars :: Expr DBVar -> Expr DBVar
simplifyVars r = applyToExpr subst r
  where subst = compress . mconcat . catMaybes $ f <$> (U.universe r)
        f (Binary BEQ (Var v) x) 
          | isLeaf x  = Just $ v |-> x
          | otherwise = Nothing
        f (Binary BEQ x (Var v))
          | isLeaf x  = Just $ v |-> x
          | otherwise = Nothing
        f _ = Nothing

-- Removes duplicate terms from the and operations at root
simplifyAnds :: (Ord a) => Expr a -> Expr a
simplifyAnds x = foldr1 (Binary And) $ simplifyAnds' x

simplifyAnds' :: (Ord a) => Expr a -> S.Set (Expr a)
simplifyAnds' (Binary And x y) = S.filter (not . isAnd) $ S.union (simplifyAnds' x) (simplifyAnds' y)
simplifyAnds' x = S.singleton x

isAnd :: Expr a -> Bool
isAnd (Binary And _ _) = True
isAnd _ = False

-- | Removes facts that always evaluate to False
removeFalseFacts :: [Rule] -> [Rule]
removeFalseFacts = filter (\x -> x ^. ruleTail /= ConstBool False)

-- | Removes duplicates of facts that appear more than once
removeDuplicateFacts :: [Rule] -> [Rule]
removeDuplicateFacts = nub

