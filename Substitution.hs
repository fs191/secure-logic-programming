module Substitution where

---------------------------------------------------------
---- Substitution map and related operations
---------------------------------------------------------

import qualified Data.Map as M
import qualified Data.Set as S

import Aexpr
import ErrorMsg
import ProgramOptions
import Rule

data Subst = Th (M.Map Var Term)

instance Show Subst where
    show (Th theta) = concat $ map (\(k,v) -> show k ++ " -> " ++ show v ++ "\n") (M.toList theta)

-- a prefix of all fresh variables
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
assignmentsToTheta :: Term -> Subst -> (Term, Subst)
assignmentsToTheta aexpr theta =
    case aexpr of

        AUnary  f x      -> let (y,theta') = processRec theta x in
                            (AUnary f y, theta')

        -- we assume in advance that LHS of an assignment is a free variable,
        -- and that there are no other assignments in RHS
        ABinary AAsgn (AVar x) y -> (AConstBool True, updateTheta x y theta)

        ABinary f x1 x2  -> let (y1,theta1) = processRec theta  x1 in
                            let (y2,theta2) = processRec theta1 x2 in
                            (ABinary f y1 y2, theta2)

        ANary f xs       -> let (ys,theta') = foldl (\(ys0, th0) x -> let (y,th) = processRec th0 x in (y:ys0, th)) ([], theta) xs in
                            (ANary f ys, theta')

        _                -> (aexpr,theta)

    where processRec theta' x = assignmentsToTheta x theta'

-- apply a substitution
applyTheta :: Subst -> Term -> Term
applyTheta theta aexpr =
    case aexpr of

        AVar      x -> evalTheta theta x
        AUnary  f x -> AUnary f $ processRec x

        -- we assume in advance that LHS of an assignment is a free variable
        -- otherwise, treat the source program as incorrect
        ABinary AAsgn (AVar x) y -> let z = processRec y in
                                    if memberTheta x theta then
                                        ABinary AEQ (evalTheta theta x) z
                                    else
                                        ABinary AAsgn (AVar x) z

        ABinary f x1 x2  -> let y1 = processRec x1 in
                            let y2 = processRec x2 in
                            ABinary f y1 y2

        ANary f xs       -> ANary f $ map (processRec) xs

        x                -> x

    where processRec x = applyTheta theta x

-- replace all variables with fresh names, store the replacement into a substitution
-- the fresh names are numbered starting from the input counter
-- TODO this would be better to implement using a state monad
refreshVariableNames :: Int -> Term -> (Int, Subst, Term)
refreshVariableNames = refreshVariableNamesRec emptyTheta

refreshVariableNamesRec :: Subst -> Int -> Term -> (Int, Subst, Term)
refreshVariableNamesRec theta c aexpr =
    case aexpr of

        AVar x      -> if memberTheta x theta then (c, theta, evalTheta theta x)
                       else
                           let freshVar = case x of
                                   Bound domain vt z -> AVar (Bound domain vt (nv ++ show c))
                                   Free z       -> AVar (Free (nv ++ show c))
                           in
                           (c+1, updateTheta x freshVar theta, freshVar)
        AConstBool x -> (c, theta, AConstBool x)
        AConstNum  x -> (c, theta, AConstNum x)
        AConstStr  x -> (c, theta, AConstStr x)

        ANary   op xs    -> let (c',theta',ys) = foldl (\(c0, theta0, ys0) y0 -> let (c1, theta1, y1) = processRec c0 theta0 y0 in (c1, theta1, ys0 ++ [y1])) (c, theta, []) xs in
                            (c', theta', ANary op ys)
        AUnary  op x     -> let (c',  theta',  y)  = processRec c  theta  x  in
                            (c', theta', AUnary op y)
        ABinary op x1 x2 -> let (c',  theta',  y1) = processRec c  theta  x1 in
                            let (c'', theta'', y2) = processRec c' theta' x2 in
                            (c'', theta'', ABinary op y1 y2)

    where processRec c theta x =refreshVariableNamesRec theta c x



