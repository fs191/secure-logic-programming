{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Substitution
  ( Subst
  , refreshExpr
  , applyToExpr
  , refreshAndApply
  , emptyTheta
  , unify
  , compress
  , (|->)
  ) where

---------------------------------------------------------
---- Substitution map and related operations
---------------------------------------------------------

import qualified Data.Map as M

import Data.Generics.Uniplate.Operations
import Data.Data
import Data.Maybe
import Data.List (nub)
import Data.Text.Prettyprint.Doc

import Control.Lens hiding (universe, transform, transformM)
import Control.Monad.State
import Control.Monad.Trans.UnionFind

import Expr
import DBClause

newtype Subst a = Th (M.Map a (Expr a))
  deriving (Semigroup, Monoid, Eq)

instance (Show a) => Show (Subst a) where
  show (Th theta) = concat $ map (\(k,v) -> show k ++ " -> " ++ show v ++ "\n") (M.toList theta)

instance (Show a, Pretty a) => Pretty (Subst a) where
  pretty (Th s) = tupled $ (\(a, b) -> pretty a <+> "->" <+> pretty b) <$> M.toList s

-- | Compresses the substitution using union-find.
compress :: (Ord a) => Subst a -> Subst a
compress (Th m) = runIdentity . runUnionFind $
  do
    let joins = [(k, v) | (k, Var v) <- M.toList m]
        vs = nub $ (M.keys m) <> [v | Var v <- M.elems m]
    points <- sequenceA $ fresh <$> vs
    let pointMap = M.fromList $ vs `zip` points
        joins' = [(pointMap M.! x, pointMap M.! y) | (x,y) <- joins]
    sequence_ $ uncurry union <$> joins'
    reprMap <- sequenceA $ repr <$> pointMap 
    descMap <- sequenceA $ descriptor <$> reprMap
    return . Th $ (\k -> fromMaybe (Var k) $ M.lookup k m) <$> descMap

-- | Unit substitution
emptyTheta :: Subst a
emptyTheta = Th $ M.empty

-- | Substitute variable `x` with term `y`
(|->) :: a -> Expr a -> Subst a
x |-> y = Th $ M.singleton x y

-- | Get the substitution term for variable `x`
evalTheta :: (Ord a) => Subst a -> a -> (Expr a)
evalTheta (Th theta) x = 
  if M.member x theta 
    then theta M.! x 
    else Var x

-- | Apply a substitution to an expression
applyToExpr :: (Data a, Ord a) => Subst a -> (Expr a) -> (Expr a)
applyToExpr theta bexpr = transform f bexpr
  where f (Var x) = evalTheta theta x
        f x       = x

-- | Rename all the variables in `e` by 
-- enumerating them and prepending the names with `prefix`
refreshExpr :: (Data a, Named a, Ord a) => String -> Expr a -> Subst a
refreshExpr prefix e = mconcat $ evalState substs (0 :: Int)
  where 
    substs = sequenceA $ f <$> nub [v | v@(Var _) <- universe e]
    f (Var v) =
      do
        i <- get
        modify (+1)
        let n = prefix <> show i
        return $ v |-> (Var $ rename n v)
    f _ = error "Expected a variable, got something else"

refreshAndApply :: (Data a, Named a, Ord a) => String -> Expr a -> Expr a
refreshAndApply prefix e = applyToExpr (refreshExpr prefix e) e

-- | Attempt to unify the two expressions. Will return Nothing if the expressions cannot be unified
unify :: (Ord a, Data a, Named a) => Expr a -> Expr a -> Maybe (Subst a)
unify x y = unify' [(x, y)]

-- See: https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm
unify' :: (Data a, Ord a) => [(Expr a, Expr a)] -> Maybe (Subst a)
-- Eliminate
unify' g@((v@(Var x), y):t)
  | v `elem` vars g = 
    do
      let s = applyToExpr $ x |-> y
      rest <- unify' $ (t & traversed . both %~ s)
      return $ (x |-> y) <> rest
-- Decompose
unify' ((Pred n xs, Pred m ys):t)
  | n == m && length xs == length ys = unify' $ xs `zip` ys <> t
  | otherwise = Nothing
-- Swap
unify' ((x, Var y):t) = unify' ((Var y, x):t)
-- Delete / conflict
unify' ((x,y):t) 
  | x == y    = unify' t
  | otherwise = Nothing
unify' [] = Just emptyTheta

--
-- Utils
--

vars :: [(Expr a, Expr a)] -> [Expr a]
vars x = filter isVar . uncurry (<>) $ unzip x

