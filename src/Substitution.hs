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
import Data.List (nub, stripPrefix, isPrefixOf)
import Data.Text.Prettyprint.Doc

import Control.Exception (assert)
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

-- | A very safe string
safeStr :: String
safeStr = "$!!?"

-- | Compresses the substitution using union-find.
compress :: Subst -> Subst
compress (Th m) = runIdentity . runUnionFind $
  do
    let joins = [(k, v) | (k, Var _ v) <- M.toList m]
        vs = nub $ (M.keys m) <> [v | Var _ v <- M.elems m]
    points <- traverse fresh vs
    let pointMap = M.fromList $ vs `zip` points
        joins' = [(pointMap M.! x, pointMap M.! y) | (x,y) <- joins]
    traverse (uncurry union) joins'
    reprMap <- traverse repr pointMap 
    descMap <- traverse descriptor reprMap
    let toMap k = M.singleton k <$> M.lookup k m
    return . Th . mconcat $ descMap ^.. folded . to toMap . _Just

-- | Unit substitution
emptyTheta :: Subst
emptyTheta = Th $ M.empty

-- | Substitute variable `x` with term `y`
(|->) :: Expr -> Expr -> Maybe Subst
v@(Var _ x) |-> y = Th <$> (M.singleton x <$> t)
  where t = unifyVars y v
_ |-> _ = Nothing

-- | Get the substitution term for variable `x`
evalTheta :: Subst -> String -> Expr
evalTheta (Th theta) x = 
  if M.member x theta 
    then theta M.! x 
    else var x

-- | Apply a substitution to an expression
-- TODO: unify type and donmain as well
applyToExpr :: Subst -> Expr -> Expr
applyToExpr theta bexpr = removeSafePrefixes . transform f $ safePrefix _bexpr
  where theta'      = mapKeys (safeStr<>) theta
        f (Var _ x) = evalTheta theta' x
        f x         = x
        _vars       = [b | (Var _ b) <- universe bexpr]
        -- Ensure that no variables begin with '#' before application
        _bexpr      = assert (all (not . isPrefixOf "#") _vars) bexpr

removeSafePrefixes :: Expr -> Expr
removeSafePrefixes = transform f
  where
    f (Var a x) = Var a $ fromMaybe x $ stripPrefix safeStr x
    f x = x

mapKeys :: (String -> String) -> Subst -> Subst
mapKeys f (Th theta) = Th $ M.mapKeys f theta

-- | Rename all the variables in `e` by 
-- enumerating them and prepending the names with `prefix`
refreshExpr :: String -> Expr -> Subst
refreshExpr prefix e = mconcat $ evalState substs (0 :: Int)
  where 
    substs = traverse f $ nub [v | v@(Var _ _) <- universe e]
    f v@(Var _ _) =
      do
        i <- get
        modify (+1)
        let n = prefix <> show i
        -- This should succeed, because the type remains the same
        return . fromMaybe undefined $ v |-> var n
    f _ = error "Expected a variable, got something else"

safePrefix :: Expr -> Expr
safePrefix = transform f
  where f (Var a n) = Var a (safeStr <> n)
        f x = x

-- | Immediately refreshes variable names in `e`
refreshAndApply :: String -> Expr -> Expr
refreshAndApply prefix e = applyToExpr (refreshExpr prefix e) e

-- | Attempt to unify the two expressions. Will return Nothing if the expressions cannot be unified
unify :: Expr -> Expr -> Maybe Subst
unify x y = unify' [(x, y)]

-- See: https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm
unify' :: [(Expr, Expr)] -> Maybe Subst
-- Eliminate
unify' g@((v@(Var _ _), y):t)
  | v `elem` vars g = 
    do
      s <- applyToExpr <$> v |-> y
      rest <- unify' $ (t & traversed . both %~ s)
      (v |-> y) <> return rest
-- Decompose
unify' ((Pred _ n xs, Pred _ m ys):t)
  | n == m && length xs == length ys = unify' $ xs `zip` ys <> t
  | otherwise = Nothing

unify' ((Expr.List _ xs, Expr.List _ ys):t)
  | length xs == length ys = unify' $ xs `zip` ys <> t
  | otherwise = Nothing

unify' ((Add _ x1 x2, Add _ y1 y2):t) = unify' $ [x1,x2] `zip` [y1,y2] <> t
unify' ((Sub _ x1 x2, Sub _ y1 y2):t) = unify' $ [x1,x2] `zip` [y1,y2] <> t
unify' ((Mul _ x1 x2, Mul _ y1 y2):t) = unify' $ [x1,x2] `zip` [y1,y2] <> t
unify' ((Div _ x1 x2, Div _ y1 y2):t) = unify' $ [x1,x2] `zip` [y1,y2] <> t
unify' ((Min _ x1 x2, Min _ y1 y2):t) = unify' $ [x1,x2] `zip` [y1,y2] <> t
unify' ((Max _ x1 x2, Max _ y1 y2):t) = unify' $ [x1,x2] `zip` [y1,y2] <> t

unify' ((Not _ x, Not _ y):t) = unify' $ [x] `zip` [y] <> t
unify' ((Neg _ x, Neg _ y):t) = unify' $ [x] `zip` [y] <> t
unify' ((Inv _ x, Inv _ y):t) = unify' $ [x] `zip` [y] <> t

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

