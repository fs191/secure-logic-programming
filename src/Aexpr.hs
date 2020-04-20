module Aexpr
  ( AExpr(..)
  , ABinOp(..), AUnOp(..), AListOp(..)
  , BExpr(..)
  , BBinOp(..), BUnOp(..), BListOp(..)
  , BBinPredOp(..), BListPredOp(..)
  , extractAllPredicates
  , foldBool
  , evalAexpr, evalBexpr
  , aexprToString, bexprToString
  , isConstAexpr, isConstBexpr
  , simplifyBool
  , ruleIndent
  ) where

---------------------------------------------------------
---- Arithmetic and Boolean expressions
---------------------------------------------------------

import Data.Hashable
import Data.List
import Debug.Trace
import qualified Data.Map as M
import qualified Data.Set as S

import ErrorMsg

-- artihmetic expressions
data AExpr a
  = AVar a
  | AConstNum Int
  | AConstStr String
  | AUnary  AUnOp   (AExpr a)
  | ABinary ABinOp  (AExpr a) (AExpr a)
  | ANary   AListOp [AExpr a]
  deriving (Ord,Eq)

instance (Show a) => Show (AExpr a) where
  show (AVar x) = show x
  show (AConstNum x) = show x
  show (AConstStr x) = show x
  show (AUnary op x) = (show op) ++ (show x)
  show (ABinary op x y) = (show x) ++ (show op) ++ (show y)
  show (ANary op l) = (show op) ++ (show l)

data AUnOp
  = ANeg
  deriving (Ord,Eq,Show)

data ABinOp
  = ADiv | AMult | AAdd | ASub
  | AMin | AMax
  deriving (Ord,Eq,Show)

data AListOp
  = ASum  | AProd
  deriving (Ord,Eq,Show)

-- boolean expressions
data BExpr a
  = BConstBool Bool
  | BBinPred  BBinPredOp (AExpr a) (AExpr a)
  | BListPred BListPredOp [AExpr a]
  | BUnary  BUnOp   (BExpr a)
  | BBinary BBinOp  (BExpr a) (BExpr a)
  | BNary   BListOp [BExpr a]
  deriving (Ord,Eq)

instance (Show a) => Show (BExpr a) where
  show (BConstBool x) = show x
  show (BBinPred op x y) = (show x) ++ (show op) ++ (show y)
  show (BListPred op l) = (show op) ++ (show l)
  show (BUnary op x) = (show op) ++ (show x)
  show (BBinary op x y) = (show x) ++ (show op) ++ (show y)
  show (BNary op l) = (show op) ++ (show l)

data BUnOp
  = BNot
  deriving (Ord,Eq)

instance Show BUnOp where
  show BNot = "-"

data BBinOp
  = BAnd | BOr
  deriving (Ord,Eq)

instance Show BBinOp where
  show BAnd = " AND "
  show BOr  = " OR "

data BBinPredOp
  = BLT | BLE | BEQ | BGE | BGT | BAsgn
  deriving (Ord,Eq)

instance Show BBinPredOp where
  show BLT   = " < "
  show BLE   = " <= "
  show BEQ   = " == "
  show BGE   = " >= "
  show BGT   = " > "
  show BAsgn = " := "

data BListPredOp
  = BPredName String
  deriving (Ord,Eq)

instance Show BListPredOp where
  show (BPredName n) = n

data BListOp
  = BAnds | BOrs
  deriving (Ord,Eq,Show)

-- this has been stolen from Data.Logic.Propositional.NormalForms and adjusted to our data types
neg :: BExpr a -> BExpr a
neg = BUnary BNot

disj :: BExpr a -> BExpr a -> BExpr a
disj = BBinary BOr

conj :: BExpr a -> BExpr a -> BExpr a
conj = BBinary BAnd

toNNF :: BExpr a -> BExpr a
toNNF (BUnary BNot (BUnary BNot expr))       = toNNF expr

toNNF (BBinary BAnd exp1 exp2)               = toNNF exp1 `conj` toNNF exp2
toNNF (BUnary BNot (BBinary BAnd exp1 exp2)) = toNNF $ neg exp1 `disj` neg exp2

toNNF (BBinary BOr exp1 exp2)               = toNNF exp1 `disj` toNNF exp2
toNNF (BUnary BNot (BBinary BOr exp1 exp2))  = toNNF $ neg exp1 `conj` neg exp2

toNNF expr                                 = expr

toCNF :: BExpr a -> BExpr a
toCNF = toCNF' . toNNF
  where
    toCNF' :: BExpr a -> BExpr a
    toCNF' (BBinary BAnd exp1 exp2) = toCNF' exp1 `conj` toCNF' exp2
    toCNF' (BBinary BOr  exp1 exp2) = toCNF' exp1 `dist` toCNF' exp2
    toCNF' expr                    = expr

    dist :: BExpr a -> BExpr a -> BExpr a
    dist (BBinary BAnd e11 e12) e2 = (e11 `dist` e2) `conj` (e12 `dist` e2)
    dist e1 (BBinary BAnd e21 e22) = (e1 `dist` e21) `conj` (e1 `dist` e22)
    dist e1 e2                    = e1 `disj` e2

toDNF :: BExpr a -> BExpr a
toDNF = toDNF' . toNNF
  where
    toDNF' :: BExpr a -> BExpr a
    toDNF' (BBinary BAnd exp1 exp2) = toDNF' exp1 `dist` toDNF' exp2
    toDNF' (BBinary BOr exp1 exp2) = toDNF' exp1 `disj` toDNF' exp2
    toDNF' expr                    = expr

    dist :: BExpr a -> BExpr a -> BExpr a
    dist (BBinary BOr e11 e12) e2 = (e11 `dist` e2) `disj` (e12 `dist` e2)
    dist e1 (BBinary BOr e21 e22) = (e1 `dist` e21) `disj` (e1 `dist` e22)
    dist e1 e2                    = e1 `conj` e2

fromDNFtoList :: (Eq a) => BExpr a -> [[BExpr a]]
fromDNFtoList expr =
    case expr of
        (BBinary BOr  exp1 exp2) -> fromDNFtoList exp1 ++ fromDNFtoList exp2
        (BBinary BAnd exp1 exp2) -> zipWith (++) (fromDNFtoList exp1) (fromDNFtoList exp2)
        x                        -> [[x]]

fromListtoDNF :: (Eq a) => [[BExpr a]] -> BExpr a
fromListtoDNF xss = BNary BOrs $ map (BNary BAnds) xss

-- simplify a boolean expression
foldBool :: (Show a, Eq a) => BExpr a -> BExpr a
foldBool expr =
    let exprDNF = toDNF expr in
    let xss = fromDNFtoList exprDNF in
    let res = fromListtoDNF xss in
    --trace ("############" ++ show expr ++ "\n") $
    --trace ("############" ++ show exprDNF ++ "\n") $
    --trace ("############" ++ show xss ++ "\n") $
    res

unfoldBool :: (Eq a) => BExpr a -> BExpr a
unfoldBool (BNary BAnds []) = BConstBool True
unfoldBool (BNary BAnds (x:xs)) =
    foldl (BBinary BAnd) (unfoldBool x) $ map unfoldBool xs

unfoldBool (BNary BOrs []) = BConstBool False
unfoldBool (BNary BOrs (x:xs)) =
    foldl (BBinary BOr) (unfoldBool x) $ map unfoldBool xs

unfoldBool x = x

simplifyFoldedBool :: (Eq a) => BExpr a -> BExpr a
simplifyFoldedBool (BNary BOrs []) = BConstBool False
simplifyFoldedBool (BNary BOrs xs) =
    let ys = map simplifyFoldedBool xs in
    if elem (BConstBool True) ys then (BConstBool True)
    else
         let zs = filter ((BConstBool False) /=) ys in
         case zs of
             []  -> BConstBool False
             [z] -> z
             _   -> BNary BOrs $ nub zs

simplifyFoldedBool (BNary BAnds []) = BConstBool True
simplifyFoldedBool (BNary BAnds xs) =
    let ys = map simplifyFoldedBool xs in
    if elem (BConstBool False) ys then (BConstBool False)
    else
         let zs = filter ((BConstBool True) /=) ys in
         case zs of
             []  -> BConstBool True
             [z] -> z
             _   -> BNary BAnds $ nub zs

simplifyFoldedBool x = x

simplifyBool :: (Show a, Eq a) => BExpr a -> BExpr a
simplifyBool expr =
    let x1 = foldBool expr in
    let x2 = simplifyFoldedBool x1 in
    let x3 = unfoldBool x2 in
    --trace ("############" ++ show x1  ++ "\n") $
    --trace ("############" ++ show x2 ++ "\n") $
    --trace ("############" ++ show x3 ++ "\n\n") $
    x3

------------------------------------------------------------------------------------
-- this is currently used only for visual feedback
ruleIndent = "  "

aexprToString :: (a -> String) -> AExpr a -> String
aexprToString f aexpr =
    case aexpr of
        AVar x -> f x
        AConstNum c -> show c
        AConstStr s -> s

        ANary ASum xs -> "(" ++ intercalate " + " (map (aexprToString f) xs) ++ ")"
        ANary AProd xs -> "(" ++ intercalate " * " (map (aexprToString f) xs) ++ ")"

        AUnary ANeg x -> "( - " ++ aexprToString f x ++ ")"

        ABinary ADiv x1 x2 -> "(" ++ aexprToString f x1 ++ " / " ++ aexprToString f x2 ++ ")"
        ABinary AMult x1 x2 -> "(" ++ aexprToString f x1 ++ " * " ++ aexprToString f x2 ++ ")"
        ABinary AAdd x1 x2 -> "(" ++ aexprToString f x1 ++ " + " ++ aexprToString f x2 ++ ")"
        ABinary ASub x1 x2 -> "(" ++ aexprToString f x1 ++ " - " ++ aexprToString f x2 ++ ")"
        ABinary AMin x1 x2  -> "min(" ++ aexprToString f x1 ++ ", " ++ aexprToString f x2 ++ ")"
        ABinary AMax x1 x2  -> "max(" ++ aexprToString f x1 ++ ", " ++ aexprToString f x2 ++ ")"

bexprToString :: String -> (a -> String) -> BExpr a -> String
bexprToString prefix f aexpr =
    case aexpr of
        BConstBool b -> case b of {True -> "true"; False -> "false"}

        BBinPred BLT x1 x2 -> "(" ++ aexprToString f x1 ++ " < " ++ aexprToString f x2 ++ ")"
        BBinPred BLE x1 x2 -> "(" ++ aexprToString f x1 ++ " =< " ++ aexprToString f x2 ++ ")"
        BBinPred BEQ x1 x2 -> "(" ++ aexprToString f x1 ++ " == " ++ aexprToString f x2 ++ ")"
        BBinPred BGE x1 x2 -> "(" ++ aexprToString f x1 ++ " >= " ++ aexprToString f x2 ++ ")"
        BBinPred BGT x1 x2 -> "(" ++ aexprToString f x1 ++ " > " ++ aexprToString f x2 ++ ")"
        BBinPred BAsgn x1 x2 -> "(" ++ aexprToString f x1 ++ " is " ++ aexprToString f x2 ++ ")"

        BListPred (BPredName pred) args -> pred ++ "(" ++ intercalate "," (map (aexprToString f) args) ++ ")"

        BNary BAnds xs -> ruleIndent ++ intercalate (",\n" ++ prefix ++ ruleIndent) (map (bexprToString prefix f) xs)
        BNary BOrs  xs -> ruleIndent ++ "(" ++ intercalate (";\n\n" ++ prefix ++ ruleIndent) (map (bexprToString prefix f) xs) ++ ")"

        BUnary BNot x -> "not(" ++ bexprToString prefix f x ++ ")"

        BBinary BAnd x1 x2 -> bexprToString prefix f x1 ++ ",\n" ++ prefix ++ ruleIndent ++ bexprToString prefix f x2
        BBinary BOr  x1 x2 -> bexprToString prefix f x1 ++ ";\n\n" ++ prefix ++ ruleIndent ++ bexprToString prefix f x2

--------------------------
-- get certain data of all variables
getAExprVars :: Ord b => (a -> b) -> AExpr a -> S.Set b
getAExprVars f aexpr =
    case aexpr of
        AVar x      -> S.singleton $ f x
        AConstNum _ -> S.empty
        AConstStr _ -> S.empty

        ANary _ xs      -> foldr S.union S.empty $ map (getAExprVars f) xs
        AUnary _ x      -> getAExprVars f x
        ABinary _ x1 x2 -> S.union (getAExprVars f x1) (getAExprVars f x2)

getBExprVars :: Ord b => (a -> b) -> BExpr a -> S.Set b
getBExprVars f bexpr =
    case bexpr of
        BConstBool _      -> S.empty
        BBinPred  _ x1 x2 -> S.union (getAExprVars f x1) (getAExprVars f x2)
        BListPred _ xs    -> foldr S.union S.empty $ map (getAExprVars f) xs

        BNary _ xs      -> foldr S.union S.empty $ map (getBExprVars f) xs
        BUnary _ x      -> getBExprVars f x
        BBinary _ x1 x2 -> S.union (getBExprVars f x1) (getBExprVars f x2)

--------------------------
-- is the expression constant (i.e. does not contain any variables)?
isConstAexpr :: AExpr a -> Bool
isConstAexpr aexpr =
    case aexpr of
        AVar x -> False

        AConstNum  _ -> True
        AConstStr  _ -> True

        ANary _ xs      -> foldl (&&) True $ map isConstAexpr xs
        AUnary _ x      -> isConstAexpr x
        ABinary _ x1 x2 -> (isConstAexpr x1) && (isConstAexpr x2)

isConstBexpr :: BExpr a -> Bool
isConstBexpr bexpr =
    case bexpr of
        BConstBool _      -> True
        BBinPred  _ x1 x2 -> (isConstAexpr x1) && (isConstAexpr x2)
        BListPred _ xs    -> foldl (&&) True $ map isConstAexpr xs

        BNary _ xs      -> foldl (&&) True $ map isConstBexpr xs
        BUnary _ x      -> isConstBexpr x
        BBinary _ x1 x2 -> (isConstBexpr x1) && (isConstBexpr x2)

--------------------------
-- evaluate an expression
evalAexpr :: (Show a) => AExpr a -> AExpr a
evalAexpr aexpr =
    case aexpr of
        AVar x       -> error $ error_nonConstantTerm (aexprToString show aexpr)
        AConstNum c  -> AConstNum c
        AConstStr c  -> AConstStr c

        ANary ASum  xs -> AConstNum $ sum $ map processRec xs
        ANary AProd xs -> AConstNum $ product $ map processRec xs

        AUnary ANeg x -> AConstNum  $ 0 - (processRec x)

        ABinary ADiv x1 x2  -> AConstNum $ div (processRec x1) (processRec x2)
        ABinary AMult x1 x2 -> AConstNum $ (processRec x1) * (processRec x2)
        ABinary AAdd x1 x2  -> AConstNum $ (processRec x1) + (processRec x2)
        ABinary ASub x1 x2  -> AConstNum $ (processRec x1) - (processRec x2)
        ABinary AMin x1 x2  -> AConstNum $ min (processRec x1) (processRec x2)
        ABinary AMax x1 x2  -> AConstNum $ max (processRec x1) (processRec x2)

    where processRec x  = let y = evalAexpr x in
                                    case y of
                                        (AConstNum c)  -> c
                                        _              -> error $ error_typeNum y


evalBexpr :: (Show a) => BExpr a -> BExpr a
evalBexpr bexpr =
    case bexpr of

        BConstBool b       -> BConstBool b

        BBinPred BLT x1 x2 -> BConstBool $ (processRecNum x1) < (processRecNum x2)
        BBinPred BLE x1 x2 -> BConstBool $ (processRecNum x1) <= (processRecNum x2)
        BBinPred BEQ x1 x2 -> BConstBool $ (processRecAny x1) == (processRecAny x2)
        BBinPred BGE x1 x2 -> BConstBool $ (processRecNum x1) >= (processRecNum x2)
        BBinPred BGT x1 x2 -> BConstBool $ (processRecNum x1) > (processRecNum x2)

        -- we get error from evalAexpr if here is a free variable, which is good
        -- alternatively, we could carry a substitution
        BBinPred BAsgn x1 x2 -> BConstBool $ (processRecAny x1) == (processRecAny x2)

        BListPred (BPredName _) _ -> error $ error_nonConstantTerm (bexprToString "" show bexpr)

        BNary BAnds xs -> BConstBool $ foldl (&&) True  $ map processRecBool xs
        BNary BOrs  xs -> BConstBool $ foldl (||) False $ map processRecBool xs

        BUnary BNot x -> BConstBool $ (not (processRecBool x))

        BBinary BAnd x1 x2 -> BConstBool $ (processRecBool x1) && (processRecBool x2)
        BBinary BOr  x1 x2 -> BConstBool $ (processRecBool x1) || (processRecBool x2)

    where processRecAny x  = let y = evalAexpr x in
                                    case y of
                                        (AConstNum c)  -> show c
                                        (AConstStr c)  -> c
          processRecNum x  = let y = evalAexpr x in
                                    case y of
                                        (AConstNum c)  -> c
                                        _              -> error $ error_typeNum y
          processRecBool x = let y = evalBexpr x in
                                    case y of
                                        (BConstBool c) -> c
                                        _              -> error $ error_typeBool y
------------------------------------------------------------------------------------
extractAllPredicates :: BExpr a -> [(String, [AExpr a])]
extractAllPredicates bexpr =
    case bexpr of

        BListPred (BPredName f) xs -> [(f,xs)]

        BNary _ xs      -> concat $ map processRec xs
        BUnary _ x      -> processRec x
        BBinary _ x1 x2 -> (processRec x1) ++ (processRec x2)
        _               -> []

    where processRec x = extractAllPredicates x

