{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Substitution
  ( Subst
  , refreshExpr
  , applyToExpr
  , refreshAndApply
  , emptyTheta
  , assignmentsToTheta
  , updateTheta
  , unify
  ) where

---------------------------------------------------------
---- Substitution map and related operations
---------------------------------------------------------

import qualified Data.Map as M

import Data.Generics.Uniplate.Operations
import Data.Data

import Control.Lens hiding (universe, transform)

import Expr
import DBClause

newtype Subst a = Th (M.Map a (Expr a))
  deriving (Semigroup, Monoid)

instance (Show a) => Show (Subst a) where
    show (Th theta) = concat $ map (\(k,v) -> show k ++ " -> " ++ show v ++ "\n") (M.toList theta)

emptyTheta :: Subst a
emptyTheta = Th $ M.empty

(|->) :: a -> Expr a -> Subst a
x |-> y = Th $ M.singleton x y

evalTheta :: (Ord a) => Subst a -> a -> (Expr a)
evalTheta (Th theta) x = 
  if M.member x theta 
    then theta M.! x 
    else Var x

-- update the substitution
updateTheta :: (Ord a) => a -> (Expr a) -> Subst a -> Subst a
updateTheta a b theta = (a |-> b) <> theta

-- extract assignments into a substitution
assignmentsToTheta :: (Ord a) => (Expr a) -> Subst a -> ((Expr a), Subst a)
assignmentsToTheta bexpr theta =
    case bexpr of

        Unary  f x      -> let (y,theta') = processRec theta x in
                            (Unary f y, theta')

        -- we assume in advance that LHS of an assignment is a free variable,
        -- and that there are no other assignments in RHS
        Binary BAsgn (Var x) y -> (ConstBool True, updateTheta x y theta)

        Binary f x1 x2  -> let (y1,theta1) = processRec theta  x1 in
                            let (y2,theta2) = processRec theta1 x2 in
                            (Binary f y1 y2, theta2)
        _                -> (bexpr,theta)

    where processRec theta' x = assignmentsToTheta x theta'

-- | Apply a substitution to an expression
applyToExpr :: (Data a, Ord a) => Subst a -> (Expr a) -> (Expr a)
applyToExpr theta bexpr = transform f bexpr
  where f (Var x) = evalTheta theta x
        f x       = x

refreshExpr :: (Data a, Named a, Ord a) => String -> Expr a -> Subst a
refreshExpr prefix e = mconcat 
  [ v |-> (Var $ rename (prefix <> name v) v)
  | (Var v) <- universe e
  ]

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

vars :: [(Expr a, Expr a)] -> [Expr a]
vars ((x@(Var _), y@(Var _)):t) = x:y:(vars t)
vars ((x@(Var _), _):t) = x:(vars t)
vars ((_, x@(Var _)):t) = x:(vars t)

