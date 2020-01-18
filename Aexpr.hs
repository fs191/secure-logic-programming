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
  | AUnary  AUnOp   (AExpr a)
  | ABinary ABinOp  (AExpr a) (AExpr a)
  | ANary   AListOp [AExpr a]
  deriving (Ord,Eq,Show)

data AUnOp
  = ANeg | ANot
  deriving (Ord,Eq,Show)

data ABinOp
  = ADiv | AMult | AAdd | ASub
  | AMin | AMax
  | AAnd | AOr
  | ALT | ALE | AEQ | AGE | AGT
  deriving (Ord,Eq,Show)

data AListOp
  = ASum  | AProd | AMins | AMaxs | AAnds | AOrs | AMember String
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

fromDNFtoList :: (Eq a) => AExpr a -> [[AExpr a]]
fromDNFtoList expr =
    case expr of
        (ABinary AOr  exp1 exp2) -> fromDNFtoList exp1 ++ fromDNFtoList exp2
        (ABinary AAnd exp1 exp2) -> zipWith (++) (fromDNFtoList exp1) (fromDNFtoList exp2)
        x                        -> [[x]]

fromListtoDNF :: (Eq a) => [[AExpr a]] -> AExpr a
fromListtoDNF xss = ANary AOrs $ map (ANary AAnds) xss

-- simplify a boolean expression
foldBool :: (Show a, Eq a) => AExpr a -> AExpr a
foldBool expr =
    let exprDNF = toDNF expr in
    let xss = fromDNFtoList exprDNF in
    let res = fromListtoDNF xss in
    --trace ("############" ++ show expr ++ "\n") $
    --trace ("############" ++ show exprDNF ++ "\n") $
    --trace ("############" ++ show xss ++ "\n") $
    res

unfoldBool :: (Eq a) => AExpr a -> AExpr a
unfoldBool (ANary AAnds []) = AConstNum 1
unfoldBool (ANary AAnds (x:xs)) =
    foldl (ABinary AAnd) (unfoldBool x) $ map unfoldBool xs

unfoldBool (ANary AOrs []) = AConstNum 0
unfoldBool (ANary AOrs (x:xs)) =
    foldl (ABinary AOr) (unfoldBool x) $ map unfoldBool xs

unfoldBool x = x

simplifyFoldedBool :: (Eq a) => AExpr a -> AExpr a
simplifyFoldedBool (ANary AOrs []) = AConstNum 0
simplifyFoldedBool (ANary AOrs xs) =
    let ys = map simplifyFoldedBool xs in
    if elem (AConstNum 1) ys then (AConstNum 1)
    else
         let zs = filter ((AConstNum 0) /=) ys in
         case zs of
             []  -> AConstNum 0
             [z] -> z
             _   -> ANary AOrs $ nub zs

simplifyFoldedBool (ANary AAnds []) = AConstNum 1
simplifyFoldedBool (ANary AAnds xs) =
    let ys = map simplifyFoldedBool xs in
    if elem (AConstNum 0) ys then (AConstNum 0)
    else
         let zs = filter ((AConstNum 1) /=) ys in
         case zs of
             []  -> AConstNum 1
             [z] -> z
             _   -> ANary AAnds $ nub zs

simplifyFoldedBool x = x

simplifyBool :: (Show a, Eq a) => AExpr a -> AExpr a
simplifyBool expr =
    let x1 = foldBool expr in
    let x2 = simplifyFoldedBool x1 in
    let x3 = unfoldBool x2 in
    --trace ("############" ++ show x1  ++ "\n") $
    --trace ("############" ++ show x2 ++ "\n") $
    --trace ("############" ++ show x3 ++ "\n\n") $
    x3

------------------------------------------------------------------------------------
-- TODO adjust this to SecreC syntax
aexprToString :: Show a => AExpr a -> String
aexprToString aexpr =
    case aexpr of
        AVar x -> show x
        AConstNum c -> show c
        AConstStr s -> s

        ANary (AMember pred) args -> pred ++ "(" ++ intercalate "," (map aexprToString args) ++ ")"

        ANary ASum xs -> "(" ++ intercalate " + " (map aexprToString xs) ++ ")"
        ANary AProd xs -> "(" ++ intercalate " * " (map aexprToString xs) ++ ")"
        ANary AMins xs -> "min(" ++ intercalate "," (map aexprToString xs) ++ ")"
        ANary AMaxs xs -> "max(" ++ intercalate "," (map aexprToString xs) ++ ")"
        ANary AAnds xs -> "(" ++ intercalate " AND " (map aexprToString xs) ++ ")"
        ANary AOrs  xs -> "(" ++ intercalate " OR " (map aexprToString xs) ++ ")"

        AUnary ANeg x -> "( - " ++ aexprToString x ++ ")"
        AUnary ANot x -> "not(" ++ aexprToString x ++ ")"

        ABinary ADiv x1 x2 -> "(" ++ aexprToString x1 ++ " / " ++ aexprToString x2 ++ ")"
        ABinary AMult x1 x2 -> "(" ++ aexprToString x1 ++ " * " ++ aexprToString x2 ++ ")"
        ABinary AAdd x1 x2 -> "(" ++ aexprToString x1 ++ " + " ++ aexprToString x2 ++ ")"
        ABinary ASub x1 x2 -> "(" ++ aexprToString x1 ++ " - " ++ aexprToString x2 ++ ")"
        ABinary AMin x1 x2 -> "min(" ++ aexprToString x1 ++ ", " ++ aexprToString x2 ++ ")"
        ABinary AMax x1 x2 -> "max(" ++ aexprToString x1 ++ ", " ++ aexprToString x2 ++ ")"
        ABinary AAnd x1 x2 -> "(" ++ aexprToString x1 ++ " AND " ++ aexprToString x2 ++ ")"
        ABinary AOr  x1 x2 -> "(" ++ aexprToString x1 ++ " OR " ++ aexprToString x2 ++ ")"
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
        AVar x      -> S.singleton $ f x
        AConstNum c -> S.empty
        AConstStr c -> S.empty

        ANary _ xs      -> foldr S.union S.empty $ map processRec xs
        AUnary _ x      -> processRec x
        ABinary _ x1 x2 -> S.union (processRec x1) (processRec x2)

    where processRec x = getAllAExprVarData f x

--------------------------
-- evaluate an aexpr
evalAexpr :: (Show a) => AExpr a -> AExpr Int
evalAexpr aexpr =
    case aexpr of
        AVar x      -> error $ error_nonConstantTerm (aexprToString aexpr)
        AConstNum c -> AConstNum c
        AConstStr c -> AConstNum $ hash c

        ANary (AMember p) xs -> error $ error_nonConstantTerm (aexprToString aexpr)

        ANary ASum  xs -> AConstNum $ sum $ map processRec xs
        ANary AProd xs -> AConstNum $ product $ map processRec xs
        ANary AMins xs -> AConstNum $ foldr min ( 999999) $ map processRec xs
        ANary AMaxs xs -> AConstNum $ foldr max (-999999) $ map processRec xs
        ANary AAnds xs -> AConstNum $ product $ map processRec xs
        ANary AOrs  xs -> AConstNum $ (\x -> if x > 0 then 1 else 0) . sum $ map processRec xs

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

        ANary op xs       -> ANary   op (map processRec xs)
        AUnary  op x      -> AUnary  op (processRec x)
        ABinary op x1 x2  -> ABinary op (processRec x1) (processRec x2)

    where processRec x = updateAexprVars dataMap f x

------------------------------------------------------------------------------------
updateAexprVarsFold :: Ord a => (a -> Int -> AExpr b) -> Int -> M.Map a (AExpr b) -> AExpr a -> (Int, M.Map a (AExpr b), AExpr b)
updateAexprVarsFold f c theta aexpr =
    case aexpr of

        AVar x      -> if M.member x theta then (c, theta, theta M.! x)
                       else
                           let freshVar = f x c in
                           (c+1, M.insert x freshVar theta, freshVar)
        AConstNum x -> (c, theta, AConstNum x)
        AConstStr x -> (c, theta, AConstStr x)

        ANary   op xs    -> let (c',theta',ys) = foldr (\y0 (c0, theta0, ys0) -> let (c1, theta1, y1) = processRec c0 theta0 y0 in (c1, theta1, (y1:ys0))) (c, theta, []) xs in
                            (c', theta', ANary op ys)
        AUnary  op x     -> let (c',  theta',  y)  = processRec c  theta  x  in
                            (c', theta', AUnary op y)
        ABinary op x1 x2 -> let (c',  theta',  y1) = processRec c  theta  x1 in
                            let (c'', theta'', y2) = processRec c' theta' x2 in
                            (c'', theta'', ABinary op y1 y2)

    where processRec c theta x = updateAexprVarsFold f c theta x

