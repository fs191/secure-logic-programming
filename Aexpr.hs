module Aexpr where

import Data.Hashable
import Data.List
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S

import ErrorMsg

-- we want to parse complex expressions and transform them to Expr afterwards
-- TODO try to add more AConst types
data AExpr a
  = AVar a
  | AConstNum Int
  | AConstStr String
  | AUnary  AUnOp (AExpr a)
  | ABinary ABinOp (AExpr a) (AExpr a)
  | ASum  [AExpr a]
  | AProd [AExpr a]
  | AMins [AExpr a]
  | AMaxs [AExpr a]
  | AAnds [AExpr a]
  | AOrs  [AExpr a]
  | AXors [AExpr a]
  deriving (Ord,Eq,Show)

data AUnOp
  = ANeg | ANot
  deriving (Ord,Eq,Show)

data ABinOp
  = ADiv | AMult | AAdd | ASub
  | AMin | AMax
  | AAnd | AOr | AXor
  | ALT | ALE | AEQ | AGE | AGT
  deriving (Ord,Eq,Show)

-- this has been stolen from Data.Logic.Propositional.NormalForms and adjusted to our data types
neg :: AExpr a -> AExpr a
neg = AUnary ANot

disj :: AExpr a -> AExpr a -> AExpr a
disj = ABinary AOr

conj :: AExpr a -> AExpr a -> AExpr a
conj = ABinary AAnd

toNNF :: AExpr a -> AExpr a
toNNF (AUnary ANot (AUnary ANot expr))       = toNNF expr

toNNF (ABinary AAnd exp1 exp2)              = toNNF exp1 `conj` toNNF exp2
toNNF (AUnary ANot (ABinary AAnd exp1 exp2)) = toNNF $ neg exp1 `disj` neg exp2

toNNF (ABinary AOr exp1 exp2)               = toNNF exp1 `disj` toNNF exp2
toNNF (AUnary ANot (ABinary AOr exp1 exp2))  = toNNF $ neg exp1 `conj` neg exp2

toNNF expr                                 = expr

toCNF :: AExpr a -> AExpr a
toCNF = toCNF' . toNNF
  where
    toCNF' :: AExpr a -> AExpr a
    toCNF' (ABinary AAnd exp1 exp2) = toCNF' exp1 `conj` toCNF' exp2
    toCNF' (ABinary AOr  exp1 exp2) = toCNF' exp1 `dist` toCNF' exp2
    toCNF' expr                    = expr
    
    dist :: AExpr a -> AExpr a -> AExpr a
    dist (ABinary AAnd e11 e12) e2 = (e11 `dist` e2) `conj` (e12 `dist` e2)
    dist e1 (ABinary AAnd e21 e22) = (e1 `dist` e21) `conj` (e1 `dist` e22)
    dist e1 e2                    = e1 `disj` e2

toDNF :: AExpr a -> AExpr a
toDNF = toDNF' . toNNF
  where
    toDNF' :: AExpr a -> AExpr a
    toDNF' (ABinary AAnd exp1 exp2) = toDNF' exp1 `dist` toDNF' exp2
    toDNF' (ABinary AOr exp1 exp2) = toDNF' exp1 `disj` toDNF' exp2
    toDNF' expr                    = expr

    dist :: AExpr a -> AExpr a -> AExpr a
    dist (ABinary AOr e11 e12) e2 = (e11 `dist` e2) `disj` (e12 `dist` e2)
    dist e1 (ABinary AOr e21 e22) = (e1 `dist` e21) `disj` (e1 `dist` e22)
    dist e1 e2                    = e1 `conj` e2

-- This does not take into account negations
fromDNFtoList :: (Show a) => AExpr a -> [[a]]
fromDNFtoList expr =
    case expr of
        (ABinary AOr  exp1 exp2) -> fromDNFtoList exp1 ++ fromDNFtoList exp2
        (ABinary AAnd exp1 exp2) -> zipWith (++) (fromDNFtoList exp1) (fromDNFtoList exp2)
        (AUnary ANot  exp)       -> fromDNFtoList exp
        (AVar x)                 -> [[x]]

-- simplify a boolean expression
simplifyBool :: (Eq a) => AExpr a -> AExpr a
simplifyBool expr =
    case expr of

        (ABinary AOr  x1 x2) -> let y1 = simplifyBool x1 in
                                let y2 = simplifyBool x2 in
                                if y1 == y2 then y1 else
                                case (y1,y2) of
                                     ((AConstNum 1), _) -> AConstNum 1
                                     (_, (AConstNum 1)) -> AConstNum 1
                                     ((AConstNum 0), x) -> x
                                     (x, (AConstNum 0)) -> x
                                     _                  -> ABinary AOr  y1 y2

        (ABinary AAnd x1 x2) -> let y1 = simplifyBool x1 in
                                let y2 = simplifyBool x2 in
                                if y1 == y2 then y1 else
                                case (y1,y2) of
                                     ((AConstNum 0), _) -> AConstNum 0
                                     (_, (AConstNum 0)) -> AConstNum 0
                                     ((AConstNum 1), x) -> x
                                     (x, (AConstNum 1)) -> x
                                     _                  -> ABinary AAnd  y1 y2

        (ABinary AXor x1 x2) -> let y1 = simplifyBool x1 in
                                let y2 = simplifyBool x2 in
                                if y1 == y2 then AConstNum 0 else
                                case (y1,y2) of
                                     ((AConstNum 1), x) -> simplifyBool (AUnary ANot x)
                                     (x, (AConstNum 1)) -> simplifyBool (AUnary ANot x)
                                     ((AConstNum 0), x) -> x
                                     (x, (AConstNum 0)) -> x
                                     _                  -> ABinary AXor  y1 y2

        (AUnary  ANot x)     -> let y = simplifyBool x in
                                case y of
                                     (AConstNum 1)    -> (AConstNum 0)
                                     (AConstNum 0)    -> (AConstNum 1)
                                     (AUnary ANot z)  -> z
                                     _                -> AUnary ANot y

        x                              -> x

------------------------------------------------------------------------------------
-- TODO adjust this to SecreC syntax
aexprToString :: AExpr String -> String
aexprToString aexpr =
    case aexpr of
        AVar x -> x
        AConstNum c -> show c
        AConstStr s -> s

        ASum xs -> "(" ++ intercalate " + " (map aexprToString xs) ++ ")"
        AProd xs -> "(" ++ intercalate " * " (map aexprToString xs) ++ ")"
        AMins xs -> "least(" ++ intercalate "," (map aexprToString xs) ++ ")"
        AMaxs xs -> "greatest(" ++ intercalate "," (map aexprToString xs) ++ ")"
        AAnds xs -> "(" ++ intercalate " AND " (map aexprToString xs) ++ ")"
        AOrs  xs -> "(" ++ intercalate " OR " (map aexprToString xs) ++ ")"
        AXors xs -> "(" ++ intercalate " OR " (map aexprToString xs) ++ ")"

        AUnary ANeg x -> "( - " ++ aexprToString x ++ ")"
        AUnary ANot x -> "not(" ++ aexprToString x ++ ")"

        ABinary ADiv x1 x2 -> "(" ++ aexprToString x1 ++ " / " ++ aexprToString x2 ++ ")"
        ABinary AMult x1 x2 -> "(" ++ aexprToString x1 ++ " * " ++ aexprToString x2 ++ ")"
        ABinary AAdd x1 x2 -> "(" ++ aexprToString x1 ++ " + " ++ aexprToString x2 ++ ")"
        ABinary ASub x1 x2 -> "(" ++ aexprToString x1 ++ " - " ++ aexprToString x2 ++ ")"
        ABinary AMin x1 x2 -> "least(" ++ aexprToString x1 ++ ", " ++ aexprToString x2 ++ ")"
        ABinary AMax x1 x2 -> "greatest(" ++ aexprToString x1 ++ ", " ++ aexprToString x2 ++ ")"
        ABinary AAnd x1 x2 -> "(" ++ aexprToString x1 ++ " AND " ++ aexprToString x2 ++ ")"
        ABinary AOr  x1 x2 -> "(" ++ aexprToString x1 ++ " OR " ++ aexprToString x2 ++ ")"
        ABinary AXor  x1 x2 -> "(" ++ aexprToString x1 ++ " OR " ++ aexprToString x2 ++ ")"
        ABinary ALT x1 x2  -> "(" ++ aexprToString x1 ++ " < " ++ aexprToString x2 ++ ")"
        ABinary ALE x1 x2  -> "(" ++ aexprToString x1 ++ " <= " ++ aexprToString x2 ++ ")"
        ABinary AEQ x1 x2  -> "(" ++ aexprToString x1 ++ " = " ++ aexprToString x2 ++ ")"
        ABinary AGE x1 x2  -> "(" ++ aexprToString x1 ++ " >= " ++ aexprToString x2 ++ ")"
        ABinary AGT x1 x2  -> "(" ++ aexprToString x1 ++ " > " ++ aexprToString x2 ++ ")"

--------------------------
-- get certain data of all variables
getAllAExprVarData :: Ord b => (a -> b) -> AExpr a -> S.Set b
getAllAExprVarData f aexpr =
    case aexpr of
        AVar x   -> S.singleton $ f x
        AConstNum c -> S.empty
        AConstStr c  -> S.empty

        ASum  xs -> foldr S.union S.empty $ map processRec xs
        AProd xs -> foldr S.union S.empty $ map processRec xs
        AMins xs -> foldr S.union S.empty $ map processRec xs
        AMaxs xs -> foldr S.union S.empty $ map processRec xs
        AAnds xs -> foldr S.union S.empty $ map processRec xs
        AOrs  xs -> foldr S.union S.empty $ map processRec xs
        AXors xs -> foldr S.union S.empty $ map processRec xs

        AUnary ANeg x -> processRec x
        AUnary ANot x -> processRec x

        ABinary ADiv x1 x2 -> S.union (processRec x1) (processRec x2)
        ABinary AMult x1 x2 -> S.union (processRec x1) (processRec x2)
        ABinary AAdd x1 x2 -> S.union (processRec x1) (processRec x2)
        ABinary ASub x1 x2 -> S.union (processRec x1) (processRec x2)
        ABinary AMin x1 x2 -> S.union (processRec x1) (processRec x2)
        ABinary AMax x1 x2 -> S.union (processRec x1) (processRec x2)
        ABinary AAnd x1 x2 -> S.union (processRec x1) (processRec x2)
        ABinary AOr  x1 x2 -> S.union (processRec x1) (processRec x2)
        ABinary AXor x1 x2 -> S.union (processRec x1) (processRec x2)
        ABinary ALT x1 x2  -> S.union (processRec x1) (processRec x2)
        ABinary ALE x1 x2  -> S.union (processRec x1) (processRec x2)
        ABinary AEQ x1 x2  -> S.union (processRec x1) (processRec x2)
        ABinary AGE x1 x2  -> S.union (processRec x1) (processRec x2)
        ABinary AGT x1 x2  -> S.union (processRec x1) (processRec x2)
    where processRec x = getAllAExprVarData f x

--------------------------
-- evaluate an aexpr
evalAexpr :: (Show a) => AExpr a -> AExpr Int
evalAexpr aexpr =
    case aexpr of
        AVar x      -> error $ error_nonGroundTerm x
        AConstNum c -> AConstNum c
        AConstStr c -> AConstNum $ hash c

        ASum  xs -> AConstNum $ sum $ map processRec xs
        AProd xs -> AConstNum $ product $ map processRec xs
        AMins xs -> AConstNum $ foldr min ( 999999) $ map processRec xs
        AMaxs xs -> AConstNum $ foldr max (-999999) $ map processRec xs
        AAnds xs -> AConstNum $ product $ map processRec xs
        AOrs  xs -> AConstNum $ (\x -> if x > 0 then 1 else 0) . sum $ map processRec xs
        AXors xs -> AConstNum $ (\x -> mod x 2) . sum $ map processRec xs

        AUnary ANeg x -> AConstNum $ 0 - (processRec x)
        AUnary ANot x -> AConstNum $ 1 - (processRec x)

        ABinary ADiv x1 x2  -> AConstNum $ div (processRec x1) (processRec x2)
        ABinary AMult x1 x2 -> AConstNum $ (processRec x1) * (processRec x2)
        ABinary AAdd x1 x2 -> AConstNum $ (processRec x1) + (processRec x2)
        ABinary ASub x1 x2 -> AConstNum $ (processRec x1) - (processRec x2)
        ABinary AMin x1 x2 -> AConstNum $ min (processRec x1) (processRec x2)
        ABinary AMax x1 x2 -> AConstNum $ max (processRec x1) (processRec x2)
        ABinary AAnd x1 x2 -> AConstNum $ (processRec x1) * (processRec x2)
        ABinary AOr  x1 x2 -> AConstNum $ if (processRec x1) + (processRec x2) > 0 then 1 else 0
        ABinary AXor x1 x2 -> AConstNum $ mod ((processRec x1) + (processRec x2)) 2
        ABinary ALT x1 x2  -> AConstNum $ if (processRec x1) <  (processRec x2) then 1 else 0
        ABinary ALE x1 x2  -> AConstNum $ if (processRec x1) <= (processRec x2) then 1 else 0
        ABinary AEQ x1 x2  -> AConstNum $ if (processRec x1) == (processRec x2) then 1 else 0
        ABinary AGE x1 x2  -> AConstNum $ if (processRec x1) >= (processRec x2) then 1 else 0
        ABinary AGT x1 x2  -> AConstNum $ if (processRec x1) >  (processRec x2) then 1 else 0
    where processRec x = let y = evalAexpr x in
                                  case y of
                                      (AConstNum c) -> c

------------------------------------------------------------------------------------
updateAexprVars :: Ord a => (M.Map a (AExpr b)) -> (a -> AExpr b) -> AExpr a -> AExpr b
updateAexprVars dataMap f aexpr =
    case aexpr of

        AVar x      -> if M.member x dataMap then dataMap M.! x else f x
        AConstNum c -> AConstNum c
        AConstStr c -> AConstStr c

        ASum  xs -> ASum  $ map processRec xs
        AProd xs -> AProd $ map processRec xs
        AMins xs -> AMins $ map processRec xs
        AMaxs xs -> AMaxs $ map processRec xs
        AAnds xs -> AAnds $ map processRec xs
        AOrs  xs -> AOrs  $ map processRec xs
        AXors xs -> AXors $ map processRec xs

        AUnary ANeg x -> AUnary ANeg $ processRec x
        AUnary ANot x -> AUnary ANot $ processRec x

        ABinary ADiv x1 x2  -> ABinary ADiv (processRec x1) (processRec x2)
        ABinary AMult x1 x2 -> ABinary AMult (processRec x1) (processRec x2)
        ABinary AAdd x1 x2 -> ABinary AAdd  (processRec x1) (processRec x2)
        ABinary ASub x1 x2 -> ABinary ASub (processRec x1) (processRec x2)
        ABinary AMin x1 x2 -> ABinary AMin (processRec x1) (processRec x2)
        ABinary AMax x1 x2 -> ABinary AMax (processRec x1) (processRec x2)
        ABinary AAnd x1 x2 -> ABinary AAnd (processRec x1) (processRec x2)
        ABinary AOr  x1 x2 -> ABinary AOr  (processRec x1) (processRec x2)
        ABinary AXor x1 x2 -> ABinary AXor (processRec x1) (processRec x2)
        ABinary ALT x1 x2  -> ABinary ALT (processRec x1) (processRec x2)
        ABinary ALE x1 x2  -> ABinary ALE (processRec x1) (processRec x2)
        ABinary AEQ x1 x2  -> ABinary AEQ (processRec x1) (processRec x2)
        ABinary AGE x1 x2  -> ABinary AGE (processRec x1) (processRec x2)
        ABinary AGT x1 x2  -> ABinary AGT (processRec x1) (processRec x2)
    where processRec x = updateAexprVars dataMap f x

