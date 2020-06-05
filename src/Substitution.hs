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

import Data.Generics.Uniplate.Data
import Data.Maybe
import Data.List (nub)
import Data.Text.Prettyprint.Doc

import Control.Lens hiding (universe, transform, transformM)
import Control.Monad.State
import Control.Monad.Trans.UnionFind

import Expr

newtype Subst = Th (M.Map String Expr)
  deriving (Semigroup, Monoid, Eq)

instance Show Subst where
  show (Th theta) = concat $ map (\(k,v) -> show k ++ " -> " ++ show v ++ "\n") (M.toList theta)

instance Pretty Subst where
  pretty (Th s) = tupled $ (\(a, b) -> pretty a <+> "->" <+> pretty b) <$> M.toList s

-- | Compresses the substitution using union-find.
compress :: Subst -> Subst
compress (Th m) = runIdentity . runUnionFind $
  do
    let joins = [(k, v) | (k, Var _ v) <- M.toList m]
        vs = nub $ (M.keys m) <> [v | Var _ v <- M.elems m]
    points <- sequenceA $ fresh <$> vs
    let pointMap = M.fromList $ vs `zip` points
        joins' = [(pointMap M.! x, pointMap M.! y) | (x,y) <- joins]
    sequence_ $ uncurry union <$> joins'
    reprMap <- sequenceA $ repr <$> pointMap 
    descMap <- sequenceA $ descriptor <$> reprMap
    return . Th $ (\k -> fromMaybe (var k) $ M.lookup k m) <$> descMap

-- | Unit substitution
emptyTheta :: Subst
emptyTheta = Th $ M.empty

-- | Substitute variable `x` with term `y`
(|->) :: String -> Expr -> Subst
x |-> y = Th $ M.singleton x y

-- | Get the substitution term for variable `x`
evalTheta :: Subst -> String -> Expr
evalTheta (Th theta) x = 
  if M.member x theta 
    then theta M.! x 
    else var x

-- | Apply a substitution to an expression
applyToExpr :: Subst -> Expr -> Expr
applyToExpr theta bexpr = transform f bexpr
  where f (Var _ x) = evalTheta theta x
        f x       = x

-- | Rename all the variables in `e` by 
-- enumerating them and prepending the names with `prefix`
refreshExpr :: String -> Expr -> Subst
refreshExpr prefix e = mconcat $ evalState substs (0 :: Int)
  where 
    substs = sequenceA $ f <$> nub [v | v@(Var _ _) <- universe e]
    f (Var _ v) =
      do
        i <- get
        modify (+1)
        let n = prefix <> show i
        return $ v |-> (var n)
    f _ = error "Expected a variable, got something else"

refreshAndApply :: String -> Expr -> Expr
refreshAndApply prefix e = applyToExpr (refreshExpr prefix e) e

-- | Attempt to unify the two expressions. Will return Nothing if the expressions cannot be unified
unify :: Expr -> Expr -> Maybe Subst
unify x y = unify' [(x, y)]

-- See: https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm
unify' :: [(Expr, Expr)] -> Maybe Subst
-- Eliminate
unify' g@((v@(Var _ x), y):t)
  | v `elem` vars g = 
    do
      let s = applyToExpr $ x |-> y
      rest <- unify' $ (t & traversed . both %~ s)
      return $ (x |-> y) <> rest
-- Decompose
unify' ((Pred _ n xs, Pred _ m ys):t)
  | n == m && length xs == length ys = unify' $ xs `zip` ys <> t
  | otherwise = Nothing
-- Swap
unify' ((x, Var _ y):t) = unify' ((var y, x):t)
-- Delete / conflict
unify' ((x,y):t) 
  | x == y    = unify' t
  | otherwise = Nothing
unify' [] = Just emptyTheta

--
-- Utils
--

vars :: [(Expr, Expr)] -> [Expr]
vars x = filter isVar . uncurry (<>) $ unzip x

