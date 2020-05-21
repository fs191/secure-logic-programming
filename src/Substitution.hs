{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Substitution
  ( Subst
  , applyToFormula
  , refreshVarNames
  , applyToTerm
  , emptyTheta
  , assignmentsToTheta
  , updateTheta
  , Named(..)
  ) where

---------------------------------------------------------
---- Substitution map and related operations
---------------------------------------------------------

import qualified Data.Map as M
import Data.Foldable

import Expr

class Named a where
  name   :: a -> String
  rename :: a -> String -> a

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

memberTheta :: (Ord a) => a -> Subst a -> Bool
memberTheta x (Th theta) = M.member x theta

-- update the substitution
updateTheta :: (Ord a) => a -> (Expr a) -> Subst a -> Subst a
updateTheta a b (Th theta) =

    let theta' = M.insert a b theta in
    Th $ M.fromList $ map (\(x,y) -> case y of
                                    Var z -> if z == a then (x,b) else (x,y)
                                    _      -> (x,y)
                          ) (M.toList theta')


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

-- apply a substitution
applyToFormula :: (Ord a) => Subst a -> (Expr a) -> (Expr a)
applyToFormula theta bexpr =
    case bexpr of

        -- we assume in advance that LHS of an assignment is a free variable
        -- otherwise, treat the source program as incorrect
        Binary BAsgn (Var x) y -> let z = applyToTerm theta y in
                                     if memberTheta x theta then
                                         Binary BEQ (evalTheta theta x) z
                                     else
                                         Binary BAsgn (Var x) z

        Binary  f x1 x2 -> Binary f (applyToTerm theta x1) (applyToTerm theta x2)
        Unary f x      -> Unary f $ applyToFormula theta x
        Binary f x1 x2 -> Binary f (applyToFormula theta x1) (applyToFormula theta x2)

        x               -> x

applyToTerm :: (Ord a) => Subst a -> (Expr a) -> (Expr a)
applyToTerm theta aexpr =
    case aexpr of

        Var      x -> evalTheta theta x

        Unary  f x      -> Unary f $ processRec x
        Binary f x1 x2  -> Binary f (processRec x1) (processRec x2)

        x                -> x

    where processRec x = applyToTerm theta x

-- TODO continue from here
-- replace all variables with fresh names, store the replacement into a substitution
-- the fresh names are numbered starting from the input counter
refreshVarNames :: (Ord a, Named a) => Expr a -> Subst a
refreshVarNames e = mconcat $ f <$> [1..] `zip` toList e
  where 
    f :: Named a => (Int, a) -> Subst a
    f (i, v) = v |-> (Var $ rename v $ "X_" ++ show i)

