module Simplify (simplify) where

---------------------------------------------------------
---- Simplifications for arithmetic/boolean expressions
---------------------------------------------------------

import Relude

import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations as U
import qualified Data.Map as M
import Data.List (union, intersect, (\\))

import Expr
import Substitution

type Bounds = (Maybe Int, Maybe Int, Maybe Int, [Int])
type Env = M.Map Text Bounds
type Subst' = (Text, M.Map Expr Expr)
type Head = ([Text], [Text])

-- | List of all bounded variable names in predicates
predBoundedVars :: [Expr] -> [Text]
predBoundedVars (e@Pred {}:es) = predicateBoundedVarNames e ++ predBoundedVars es
predBoundedVars (_:es) = predBoundedVars es
predBoundedVars [] = []

-- | List of all free variable names in predicates
predFreeVars :: [Expr] -> [Text]
predFreeVars (e@Pred {}:es) = predicateFreeVarNames e ++ predFreeVars es
predFreeVars (_:es) = predFreeVars es
predFreeVars [] = []

-- a customized variant of "M.f" functions that lift variables to expressions
-- we need this since we want to omit the annotation from the key and allow to substitute free with bounded
m_member :: Expr -> M.Map Expr Expr -> Bool
m_member expr m =
  case expr of
    Var _ v -> M.member (var v) m
    _       -> M.member expr m

m_get :: M.Map Expr Expr -> Expr -> Expr
m_get m expr =
  case expr of
    Var _ v -> m M.! (var v)
    _       -> m M.! expr

m_insert :: Expr -> Expr -> M.Map Expr Expr -> M.Map Expr Expr
m_insert expr value m =
  case expr of
    Var _ v -> M.insert (var v) value m
    _       -> M.insert expr value m

m_lookup :: Expr -> M.Map Expr Expr -> Maybe Expr
m_lookup expr m =
  case expr of
    Var _ v -> M.lookup (var v) m
    _       -> M.lookup expr m

-- | Simplifies list of expressions
-- | Note that bounded vars are united with free vars (and the other way around) on purpose
simplify :: [Expr] -> Head -> [Expr]
simplify es (b, o) =
     let res = simplify' es (b `union` predFreeVars es, o `union` predBoundedVars es) in
     case res of
       Just x  -> x
       Nothing -> [eFalse]

simplify' :: [Expr] -> Head -> Maybe [Expr]
simplify' es h
  | t == Nothing = Nothing
  | otherwise =
    if Just es == substituted then
      case simplified of
        Just [] -> Just [eTrue]
        Just s  -> Just s
        Nothing -> Nothing
    else
      flip simplify' h =<< substituted
    where
      t = transformExprs es h
      substituted = flip substitute h <$> t
      simplified = findError es M.empty

-- | Evaluates and applies unification where possible
transformExprs :: [Expr] -> Head -> Maybe [Expr]
transformExprs ((Un _ l r):es) h =
  case unify l r of
    Nothing -> Nothing
    Just th ->
      if t /= Nothing
        then t <> transformExprs es h
      else Nothing
      where
        t = uns (zipWith eUn (keys th) (elems th)) h
transformExprs (e@(Is a l r):es) h@(b, o)
  | not (isLeaf l) = Nothing
  | isConstExpr l && isConstExpr r =
    if simplifyConst e == eTrue
      then transformExprs es h
      else Nothing
  | isConstExpr r = Just [Un a l (simplifyConst r)] <> transformExprs es h
  | unknowns r b == 0 = Just [e] <> transformExprs es h'
  | otherwise = Just [e] <> transformExprs es h
    where h' = if isConstExpr l then h else (_varName l : b, o)
transformExprs (e:es) h 
  | isComparison e = comparison (e:|es) h
  | otherwise      = Just [e] <> transformExprs es h
transformExprs [] _ = Just []

comparison :: NonEmpty Expr -> Head -> Maybe [Expr]
comparison (e:|es) h
  | isConstExpr e =
    if simplifyConst e == eTrue
      then transformExprs es h
      else Nothing
  | otherwise = Just [e] <> transformExprs es h

uns :: [Expr] -> Head -> Maybe [Expr]
uns (u@(Un a l@(Var _ x) r@(Var _ y)):es) h@(b, o)
  | elem y o && notElem x o ||
    elem x b && notElem y b = (Un a r l :) <$> uns es h
  | elem x b && not (isLeaf r) = Nothing
  | otherwise = (u:) <$> uns es h
uns (e:es) h = (e:) <$> uns es h
uns [] _ = Just []

unknowns :: Expr -> [Text] -> Int
unknowns e excl = length [x | (Var _ x) <- universe e, notElem x excl]

substitute :: [Expr] -> Head -> [Expr]
substitute = substitute' 1

substitute' :: Int -> [Expr] -> Head -> [Expr]
substitute' i es h =
  if substituted == es && i /= 8
    then substitute' (i + 1) es h
  else substituted
  where
    (s, es') = getSubst' i es h
    substituted = substitute'' es' s h

substitute'' :: [Expr] -> Subst' -> Head -> [Expr]
substitute'' (e@(Un a l r):es) subst@(_, m) (b, o) =
  (case intersect (varNames l) (b ++ o) of
    [] | m_member l m && (m `m_get` l) == r -> e
       | otherwise -> replace m b e
    _ -> Un a l (replace m b r))
  : substitute'' es subst (b, o)
substitute'' (e@(Is a l r):es) subst@(ann, m) (b, o) =
  (case m_lookup l m of
    Just x | x == r -> e
           | ann == "un" -> replace m b e
           | isLeaf x && isConstExpr x -> replace m b e
           | otherwise -> Is a l (replace m b r)
    _ -> Is a l (replace m b r))
  : substitute'' es subst (b, o)
substitute'' (e@Pred {}:es) subst h = e : substitute'' es subst h
substitute'' (e:es) subst h =
  simplifyConst (replace (snd subst) (fst h) e)
    : substitute'' es subst h
substitute'' [] _ _ = []

replace :: M.Map Expr Expr -> [Text] -> Expr -> Expr
replace m b = transform f
  where
    f v@(Var _ x) =
      if m_member v m then
        if elem x b && isVar (m `m_get` v) && elem (_varName (m `m_get` v)) b
          then v
          else m `m_get` v
      else v
    f e =
      if m_member e m
        then m `m_get` e
        else e

getSubst' :: Int -> [Expr] -> Head -> (Subst', [Expr])
getSubst' i es (b, o)
  | null substList && i /= 8 = getSubst' (i + 1) es (b, o)
  | otherwise = (subst, es \\ redundant)
  where
    substList =
      case i of
        1 -> [e | e@(Un _ _ y) <- es, isConstExpr y]
        2 -> [e | e@(Un _ _ y) <- es, unknowns y b == 0]
        3 -> [e | e@(Un _ _ y) <- es, null $ intersect (varNames y) b]
        4 -> [e | e@Un {} <- es]
        5 -> [e | e@(Eq _ (Var _ x) y) <- es, not (elem x b), unknowns y b == 0]
        6 -> [e | e@(Is _ (Var _ _) y) <- es, unknowns y b == 0]
        7 -> [e | e@(Is _ (Var _ _) y) <- es, null $ intersect (varNames y) b]
        _ -> [e | e@(Is _ (Var _ _) _) <- es]
    (substMap, redundant) = makeSubst substList (b ++ o) M.empty []
    subst = (if i < 5 then "un" else "is", substMap)

-- | Makes substitution map from list of expressions that can be used to substitute
makeSubst :: [Expr] -> [Text] -> M.Map Expr Expr -> [Expr] -> (M.Map Expr Expr, [Expr])
makeSubst (e@(Un _ l r):es) h m rd = makeSubst es h m' (if isRedundant then rd ++ [e] else rd)
  where (m', isRedundant) = makeSubst' l r h m
makeSubst (e@(Is _ l r):es) h m rd = makeSubst es h m' (if isRedundant then rd ++ [e] else rd)
  where (m', isRedundant) = makeSubst' l r h m
makeSubst (e@(Eq _ l r):es) h m rd = makeSubst es h m' (if isRedundant then rd ++ [e] else rd)
  where (m', isRedundant) = makeSubst' l r h m
makeSubst (_:es) h m rd = makeSubst es h m rd
makeSubst [] _ m rd = (m, rd)

makeSubst' :: Expr -> Expr -> [Text] -> M.Map Expr Expr -> (M.Map Expr Expr, Bool)
makeSubst' l r h m = (m', isRedundant && not containsRmd)
  where
  isRedundant = null [x | Var _ x <- universe l, elem x h || m_member l m]
  containsRmd = not $ null [x | v@(Var _ x) <- universe r, m_member v m]
  m' = if m_member l m || containsRmd then m else m_insert l r m

-- | Checks for conflict in a list of expressions
findError :: [Expr] -> Env -> Maybe [Expr]
findError (e:es) m
  | e' == eTrue = findError es m'
  | e' == eFalse = Nothing
  | otherwise = Just [e'] <> findError es m'
  where
    (e', m') = simplifyExpr e m
findError [] _ = Just []

-- | Simplifies and checks for conflict in expression
simplifyExpr :: Expr -> Env -> (Expr, Env)
simplifyExpr (And a l r) m = (simplifyConst (And a l' r'), m'')
  where
    (l', m') = simplifyExpr l m
    (r', m'') = simplifyExpr r m'
simplifyExpr (Or a l r) m = (simplifyConst (Or a l' r'), m'')
  where
    (l', m') = simplifyExpr l m
    (r', m'') = simplifyExpr r m'
simplifyExpr e@(Un _ x y) m = (if eq == eFalse then eq else e, m')
  where
    (eq, m') = simplifyExpr (equal x y) m
simplifyExpr (Is a x y) m = (is eq, m')
  where
    (eq, m') = simplifyExpr (equal x y) m
    is (Eq _ l r) = Is a l r
    is e = e

{-
TODO add support for non-int datatype for full integration

simplifyExpr e@(Eq _ x y) m
  | (ex == ey && isJust ex) || x == y = (eTrue, m)
  | isNothing bounds = (eFalse, m)
  | otherwise = (simplifyConst e, m'')
  where
    xb@(_, ex, _, _) = if isConstExpr x then intBounds x else getBounds x m
    yb@(_, ey, _, _) = if isConstExpr y then intBounds y else getBounds y m
    bounds = eqBounds xb yb
    m' = if isVar x then M.insert (_varName x) (fromJust bounds) m else m
    m'' = if isVar y then M.insert (_varName y) (fromJust bounds) m' else m'

simplifyExpr e@(Neq _ x y) m
  | ex /= ey && isJust ex && isJust ey = (eTrue, m)
  | isNothing bounds = (eFalse, m)
  | otherwise = (simplifyConst e, m'')
  where
    xb@(_, ex, _, _) = if isConstExpr x then intBounds x else getBounds x m
    yb@(_, ey, _, _) = if isConstExpr y then intBounds y else getBounds y m
    bounds = neqBounds xb yb
    m' = if isVar x then M.insert (_varName x) (fromJust bounds) m else m
    m'' = if isVar y then M.insert (_varName y) (fromJust bounds) m' else m'

simplifyExpr (Lt a x y) m = lessThan True a x y m
simplifyExpr (Le a x y) m = lessThan False a x y m
simplifyExpr e@Gt {} m = (switch lt, m') where (lt, m') = simplifyExpr (switch e) m
simplifyExpr e@Ge {} m = (switch le, m') where (le, m') = simplifyExpr (switch e) m
-}
simplifyExpr e m = (simplifyConst e, m)


-- | Rewrites constant terms to simpler terms
simplifyConst :: Expr -> Expr
simplifyConst = U.rewrite f
  where
    f (And _ (ConstBool _ True) x) = Just x
    f (And _ x (ConstBool _ True)) = Just x
    f (And _ (ConstBool _ False) _) = Just $ constBool False
    f (And _ _ (ConstBool _ False)) = Just $ constBool False
    f (Or _ (ConstBool _ True) _) = Just $ constBool True
    f (Or _ _ (ConstBool _ True)) = Just $ constBool True
    f (Or _ (ConstBool _ False) x) = Just x
    f (Or _ x (ConstBool _ False)) = Just x
    f (Add _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x + y
    f (Sub _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x - y
    f (Mul _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x * y
    f (Mod _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x `mod` y
    --f (Min _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x `min` y
    --f (Max _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x `max` y
    f (Lt _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x < y
    f (Gt _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x > y
    f (Le _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x <= y
    f (Ge _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x >= y
    f (Eq _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x == y
    f (Eq _ (Var _ x) (Var _ y))
      | x == y    = Just $ constBool True
      | otherwise = Nothing
    f (Neq _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x /= y
    f (Is _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x == y
    f _ = Nothing
