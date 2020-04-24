module Substitution
  ( Subst
  , applyToFormula
  , refreshVarNames
  , applyToTerm
  , emptyTheta
  , assignmentsToTheta
  , updateTheta
  ) where

---------------------------------------------------------
---- Substitution map and related operations
---------------------------------------------------------

import qualified Data.Map as M
import qualified Data.Set as S

import Aexpr
import ErrorMsg
import Rule

newtype Subst = Th (M.Map Var Term)

instance Show Subst where
    show (Th theta) = concat $ map (\(k,v) -> show k ++ " -> " ++ show v ++ "\n") (M.toList theta)

-- a prefix of all fresh variables
nv :: String
nv = "_X"

emptyTheta :: Subst
emptyTheta = Th $ M.empty

evalTheta :: Subst -> Var -> Term
evalTheta (Th theta) x = if M.member x theta then theta M.! x else AVar x

memberTheta :: Var -> Subst -> Bool
memberTheta x (Th theta) = M.member x theta

-- update the substitution
updateTheta :: Var -> Term -> Subst -> Subst
updateTheta a b (Th theta) =

    let theta' = M.insert a b theta in
    Th $ M.fromList $ map (\(x,y) -> case y of
                                    AVar z -> if z == a then (x,b) else (x,y)
                                    _      -> (x,y)
                          ) (M.toList theta')


-- extract assignments into a substitution
assignmentsToTheta :: Formula -> Subst -> (Formula, Subst)
assignmentsToTheta bexpr theta =
    case bexpr of

        BUnary  f x      -> let (y,theta') = processRec theta x in
                            (BUnary f y, theta')

        -- we assume in advance that LHS of an assignment is a free variable,
        -- and that there are no other assignments in RHS
        BBinPred BAsgn (AVar x) y -> (BConstBool True, updateTheta x y theta)

        BBinary f x1 x2  -> let (y1,theta1) = processRec theta  x1 in
                            let (y2,theta2) = processRec theta1 x2 in
                            (BBinary f y1 y2, theta2)

        BNary f xs       -> let (ys,theta') = foldl (\(ys0, th0) x -> let (y,th) = processRec th0 x in (y:ys0, th)) ([], theta) xs in
                            (BNary f ys, theta')

        _                -> (bexpr,theta)

    where processRec theta' x = assignmentsToTheta x theta'

-- apply a substitution
applyToFormula :: Subst -> Formula -> Formula
applyToFormula theta bexpr =
    case bexpr of

        -- we assume in advance that LHS of an assignment is a free variable
        -- otherwise, treat the source program as incorrect
        BBinPred BAsgn (AVar x) y -> let z = applyToTerm theta y in
                                     if memberTheta x theta then
                                         BBinPred BEQ (evalTheta theta x) z
                                     else
                                         BBinPred BAsgn (AVar x) z

        BBinPred  f x1 x2 -> BBinPred f (applyToTerm theta x1) (applyToTerm theta x2)
        BListPred f xs    -> BListPred f $ map (applyToTerm theta) xs

        BUnary f x      -> BUnary f $ applyToFormula theta x
        BBinary f x1 x2 -> BBinary f (applyToFormula theta x1) (applyToFormula theta x2)
        BNary f xs      -> BNary f $ map (applyToFormula theta) xs

        x               -> x

applyToTerm :: Subst -> Term -> Term
applyToTerm theta aexpr =
    case aexpr of

        AVar      x -> evalTheta theta x

        AUnary  f x      -> AUnary f $ processRec x
        ABinary f x1 x2  -> ABinary f (processRec x1) (processRec x2)
        ANary f xs       -> ANary f $ map processRec xs

        x                -> x

    where processRec x = applyToTerm theta x

-- TODO continue from here
-- replace all variables with fresh names, store the replacement into a substitution
-- the fresh names are numbered starting from the input counter
-- TODO this would be better to implement using a state monad
refreshVarNames :: Int -> Formula -> (Int, Subst, Formula)
refreshVarNames = refreshVarNamesRec emptyTheta

refreshVarNamesRec :: Subst -> Int -> BExpr Var -> (Int, Subst, Formula)
refreshVarNamesRec theta c bexpr =
    case bexpr of

        BConstBool x -> (c, theta, BConstBool x)

        BBinPred op x1 x2 -> let (c',  theta',  y1) = refreshVarNamesRecTerm theta  c  x1 in
                            let (c'', theta'', y2)  = refreshVarNamesRecTerm theta' c' x2 in
                            (c'', theta'', BBinPred op y1 y2)
        BListPred op xs    -> let (c',theta',ys) = foldl (\(c0, theta0, ys0) y0 -> let (c1, theta1, y1) = refreshVarNamesRecTerm theta0 c0 y0 in (c1, theta1, ys0 ++ [y1])) (c, theta, []) xs in
                            (c', theta', BListPred op ys)

        BNary   op xs    -> let (c',theta',ys) = foldl (\(c0, theta0, ys0) y0 -> let (c1, theta1, y1) = refreshVarNamesRec theta0 c0 y0 in (c1, theta1, ys0 ++ [y1])) (c, theta, []) xs in
                            (c', theta', BNary op ys)
        BUnary  op x     -> let (c',  theta',  y)  = refreshVarNamesRec theta c x  in
                            (c', theta', BUnary op y)
        BBinary op x1 x2 -> let (c',  theta',  y1) = refreshVarNamesRec theta c x1 in
                            let (c'', theta'', y2) = refreshVarNamesRec theta' c' x2 in
                            (c'', theta'', BBinary op y1 y2)

refreshVarNamesRecTerm :: Subst -> Int -> Term -> (Int, Subst, Term)
refreshVarNamesRecTerm theta c aexpr =
    case aexpr of

        AVar x      -> if memberTheta x theta then (c, theta, evalTheta theta x)
                       else
                           let freshVar = AVar $ rename (nv ++ show c) x in
                           (c+1, updateTheta x freshVar theta, freshVar)
        AConstNum  x -> (c, theta, AConstNum x)
        AConstStr  x -> (c, theta, AConstStr x)

        ANary   op xs    -> let (c',theta',ys) = foldl (\(c0, theta0, ys0) y0 -> let (c1, theta1, y1) = processRec c0 theta0 y0 in (c1, theta1, ys0 ++ [y1])) (c, theta, []) xs in
                            (c', theta', ANary op ys)
        AUnary  op x     -> let (c',  theta',  y)  = processRec c  theta  x  in
                            (c', theta', AUnary op y)
        ABinary op x1 x2 -> let (c',  theta',  y1) = processRec c  theta  x1 in
                            let (c'', theta'', y2) = processRec c' theta' x2 in
                            (c'', theta'', ABinary op y1 y2)

    where processRec c theta x =refreshVarNamesRecTerm theta c x



