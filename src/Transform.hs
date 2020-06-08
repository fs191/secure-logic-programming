{-# LANGUAGE ScopedTypeVariables #-}

module Transform
  ( deriveAllGroundRules
  ) where

---------------------------------------------------------
---- Transformation of a Datalog script
----   to intermediate representation
---------------------------------------------------------

import Data.Data.Lens as D
import Data.Generics.Uniplate.Operations as U
import Data.Maybe
import Data.List

import Control.Applicative
import Control.Lens as L

import Rule
import Expr
import Substitution
import qualified DatalogProgram as DP

-- | Generates all possible ground rules for n iterations
deriveAllGroundRules :: DP.DatalogProgram -> Int -> DP.DatalogProgram
deriveAllGroundRules program n = program'
                                   & DP.ruleLens %~ f
  where
    clauses = program ^. DP.dbClauseLens
    -- Input program but db clauses are converted to rules
    program' = program & DP.ruleLens %~ ((dbClauseToRule <$> clauses) <>)
    f :: [Rule] -> [Rule]
    f x = foldl (.) id (replicate n pipeline) x
    pipeline = 
      removeDuplicateFacts .
      removeFalseFacts .
      liftA simplify . 
      --(traversed . ruleTail %~ simplifyAnds) .
      (traversed . ruleTail %~ U.transform bindArgColumns) .
      (traversed %~ simplifyVars) . 
      inlineOnce

-- | Tries to unify each predicate in each rule body with an appropriate rule
inlineOnce :: [Rule] -> [Rule]
inlineOnce rs =
  rs <> do
    tgt <- rs
    src <- rs
    let shd = src ^. ruleHead
    let stl = src ^. ruleTail
    let ttl = tgt ^. ruleTail
    (p@(Pred _ _ _), mut) <- U.contexts ttl
    let subst = unify shd p
    s <- maybeToList subst
    return . applySubst s $ tgt & ruleTail .~ mut stl

-- | Rewrites constant terms to simpler terms
simplify :: Rule -> Rule
simplify r = r & ruleTail %~ U.rewrite f
  where 
    f (And _ (ConstBool _ True) x) = Just x
    f (And _ x (ConstBool _ True)) = Just x
    f (And _ (ConstBool _ False) _) = Just $ constBool False
    f (And _ _ (ConstBool _ False)) = Just $ constBool False
    f (Or _ (ConstBool _ True) _) = Just $ constBool True
    f (Or _ _ (ConstBool _ True)) = Just $ constBool True
    f (Or _ (ConstBool _ False) x) = Just x
    f (Or _ x (ConstBool _ False)) = Just x
    f (Add _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x + y
    f (Sub _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x - y
    f (Mul _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x * y
    f (Min _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x `min` y
    f (Max _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x `max` y
    f (Lt _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x < y
    f (Gt _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x > y
    f (Le _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x <= y
    f (Ge _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x >= y
    f (Eq _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x == y
    f (Eq _ (Var _ x) (Var _ y))
      | x == y    = Just $ constBool True
      | otherwise = Nothing
    f _ = Nothing

simplifyVars :: Rule -> Rule
simplifyVars r = applySubst subst r
  where subst = simplifyVars' $ r ^. ruleTail

-- | Removes any unnecessary variable equalities (e.g. X=Y)
simplifyVars' :: Expr -> Subst
simplifyVars' r = compress . mconcat . catMaybes $ f <$> (U.universe r)
  where f (Eq _ (Var _ v) x) 
          | isLeaf x  = Just $ v |-> x
          | otherwise = Nothing
        f (Eq _ x (Var _ v))
          | isLeaf x  = Just $ v |-> x
          | otherwise = Nothing
        f _ = Nothing

-- Removes duplicate terms from AND operations at the root expression
-- TODO: find out what's causing it to do weird substitutions in the market.pl example
--simplifyAnds :: (Ord a) => Expr a -> Expr a
--simplifyAnds x = foldr1 (Binary And) $ simplifyAnds' x
--
--simplifyAnds' :: (Ord a) => Expr a -> S.Set (Expr a)
--simplifyAnds' (Binary And x y) = S.filter (not . isAnd) $ S.union (simplifyAnds' x) (simplifyAnds' y)
--simplifyAnds' x = S.singleton x
--
--isAnd :: Expr a -> Bool
--isAnd (Binary And _ _) = True
--isAnd _ = False

-- | Removes facts that always evaluate to False
removeFalseFacts :: [Rule] -> [Rule]
removeFalseFacts = filter (\x -> x ^. ruleTail /= constBool False)

-- | Removes duplicates of facts that appear more than once
removeDuplicateFacts :: [Rule] -> [Rule]
removeDuplicateFacts = nub

-- | Binds columns that are arguments of some predicate to a new variable
bindArgColumns :: Expr -> Expr
bindArgColumns (Pred ann n as) = foldr eAnd newPred eqs
  where
    -- Generate a list of fresh variables
    varNames = (\i -> var $ "_C_" <> show i) <$> [0..]
    cols :: [(Expr, Expr)]
    cols = nub [c | c@(DBCol _ _) <- as] `zip` varNames
    -- Generate equalities between columns and their variables
    eqs     = uncurry equal <$> cols
    -- Generate a new predicate that does not have any column arguments
    f c@(DBCol _ _) = fromJust $ lookup c cols
    f c = c
    newPred = Pred ann n $ f <$> as
bindArgColumns x = x

