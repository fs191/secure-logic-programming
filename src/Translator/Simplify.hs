module Translator.Simplify (simplify) where

---------------------------------------------------------
---- Simplifications for arithmetic/boolean expressions
---------------------------------------------------------

import Relude

import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations as U
import qualified Data.Map as M
import qualified Data.List as L
import Data.Text.Prettyprint.Doc
import Data.Maybe

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
simplify es0 (b, o) =
     let es = map simplifyConst es0 in
     let res = simplify' es (b `L.union` predFreeVars es, o `L.union` predBoundedVars es) in
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
      let t' = transformExprs es h in
      if isJust t && isJust t'
        then t <> t'
        else Nothing
      where
        t = uns (zipWith eUn (keys th) (elems th)) h
transformExprs (e@(Is a l r):es) h@(b, o)
  | not (isLeaf l) = Nothing
  | isConstExpr l && isConstExpr r =
    if simplifyConst e == eTrue
      then transformExprs es h
      else Nothing
  | isConstExpr r = (Un a l (simplifyConst r):) <$> transformExprs es h
  | unknowns r b == 0 = (e:) <$> transformExprs es head'
  | otherwise = (e:) <$> transformExprs es h
    where head' = if isConstExpr l then h else (_varName l : b, o)
transformExprs e@(Lt {}:es) h = comparison e h
transformExprs e@(Le {}:es) h = comparison e h
transformExprs e@(Eq {}:es) h = comparison e h
transformExprs e@(Neq {}:es) h = comparison e h
transformExprs e@(Or {}:es) h = comparison e h
transformExprs e@(Gt {}:es) h = comparison e h
transformExprs e@(Ge {}:es) h = comparison e h
transformExprs (e:es) h = (e:) <$> transformExprs es h
transformExprs [] _ = Just []

comparison :: [Expr] -> Head -> Maybe [Expr]
comparison (e:es) h
  | isConstExpr e =
    if simplifyConst e == eTrue
      then transformExprs es h
      else Nothing
  | otherwise = (e:) <$> transformExprs es h

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
  (case L.intersect (varNames l) (b ++ o) of
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
substitute'' (e@Pred {}:es)  subst h = e : substitute'' es subst h
substitute'' (e@Query {}:es) subst h = e : substitute'' es subst h
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
  | otherwise = (subst, es L.\\ redundant)
  where
    substList =
      case i of
        1 -> [e | e@(Un _ _ y) <- es, isConstExpr y]
        2 -> [e | e@(Un _ _ y) <- es, unknowns y b == 0]
        3 -> [e | e@(Un _ _ y) <- es, null $ L.intersect (varNames y) b]
        4 -> [e | e@Un {} <- es]
        5 -> [e | e@(Eq _ (Var _ x) y) <- es, notElem x b, unknowns y b == 0]
        6 -> [e | e@(Is _ (Var _ _) y) <- es, unknowns y b == 0]
        7 -> [e | e@(Is _ (Var _ _) y) <- es, null $ L.intersect (varNames y) b]
        8 -> [e | e@(Is _ (Var _ _) _) <- es]
        _ -> error "Unexpected case"
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
  | otherwise = (e':) <$> findError es m'
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

--TODO add support for non-int datatypes (eq for strings, all inequalities for floats)
simplifyExpr e@(Eq _ x@(ConstInt _ _) y@(ConstInt _ _)) m
  | (ex == ey && isJust ex) || x == y = (eTrue, m)
  | isNothing bounds = (eFalse, m)
  | otherwise = (simplifyConst e, m'')
  where
    xb@(_, ex, _, _) = if isConstExpr x then intBounds x else getBounds x m
    yb@(_, ey, _, _) = if isConstExpr y then intBounds y else getBounds y m
    bounds = eqBounds xb yb
    m' = if isVar x then M.insert (_varName x) (fromJust bounds) m else m
    m'' = if isVar y then M.insert (_varName y) (fromJust bounds) m' else m'

simplifyExpr e@(Neq _ x@(ConstInt _ _) y@(ConstInt _ _)) m
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
simplifyExpr e m = (simplifyConst e, m)

-- | Checks that given bounds do not contradict each other
legal :: Bounds -> Bool
legal (l, e, g, n) = checkLt && checkEq && checkGt
  where
    checkLt = isNothing l || (min' l e == l && min' (add l (Just 1)) g /= g)
    checkEq = isNothing e || notElem (fromJust e) n
    checkGt = isNothing g || isNothing e || (min' g e == e)

min' :: Maybe Int -> Maybe Int -> Maybe Int
min' (Just x) (Just y) = Just $ min x y
min' x y = if isNothing x then y else x

max' :: Maybe Int -> Maybe Int -> Maybe Int
max' (Just x) (Just y) = Just $ max x y
max' x y = if isNothing x then y else x

-- | Helper funtion that simplifies an inequality
lessThan :: Bool -> Ann -> Expr -> Expr -> Env -> (Expr, Env)
lessThan strict ann x y m
  | isJust ex && isJust ey && fromJust ex < fromJust ey + fromEnum strict = (eTrue, m)
  | isNothing xb' || isNothing yb' = (eFalse, m)
  | otherwise = (e, m'')
  where
    e = simplifyConst $ if strict then Lt ann x y else Le ann x y
    xb@(_, ex, _, _) = if isConstExpr x then intBounds x else getBounds x m
    yb@(_, ey, _, _) = if isConstExpr y then intBounds y else getBounds y m
    xb' = if strict then ltBounds xb yb else leBounds xb yb
    yb' = if strict then gtBounds yb xb else geBounds yb xb
    m' = if isVar x then M.insert (_varName x) (fromJust xb') m else m
    m'' = if isVar y then M.insert (_varName y) (fromJust yb') m' else m'

-- | Switches left and right hand sides of an expression
switch :: Expr -> Expr
switch (Eq a l r) = Eq a r l
switch (Lt a l r) = Gt a r l
switch (Gt a l r) = Lt a r l
switch (Le a l r) = Ge a r l
switch (Ge a l r) = Le a r l
switch e = e

-- | Tries to construct bounds for an (arithmetic) expression
getBounds :: Expr -> Env -> Bounds
getBounds (Var _ x) m =
  if M.member x m
    then m M.! x
    else (Nothing, Nothing, Nothing, [])
getBounds c@(ConstInt _ _) m = intBounds c
getBounds (Add _ x y) m =
  (add (add lx ly) (Just 1), add ex ey, sub (add gx gy) (Just 1), n)
  where
    (lx, ex, gx, nx) = getBounds x m
    (ly, ey, gy, ny) = getBounds y m
    n = (if isJust ex then [a + (fromJust ex) | a <- ny] else [])
         `L.union` (if isJust ey then [a + (fromJust ey) | a <- nx] else [])
         `L.union` [a + b | a <- nx, b <- ny]
getBounds (Sub _ x y) m =
  (add (sub lx gy) (Just 1), sub ex ey, sub (sub gx ly) (Just 1), [a - b | a <- nx, b <- ny])
  where
    (lx, ex, gx, nx) = getBounds x m
    (ly, ey, gy, ny) = getBounds y m
    n = (if isJust ex then [a + (fromJust ex) | a <- ny] else [])
         `L.union` (if isJust ey then [a + (fromJust ey) | a <- nx] else [])
         `L.union` [a + b | a <- nx, b <- ny]
getBounds (Mul _ x y) m
  | isJust ex && isJust ey = intBounds $ constInt (fromJust ex * fromJust ey)
  | elem Nothing [lx, gx, ly, gy] = (Nothing, Nothing, Nothing, [a * b | a <- nx, b <- ny])
  | otherwise = (Just l, e, Just g, [a * b | a <- nx, b <- ny])
  where
    (lx, ex, gx, nx) = getBounds x m
    (ly, ey, gy, ny) = getBounds y m
    (lx', ly', gx', gy') = (fromJust lx + 1, fromJust ly + 1, fromJust gx - 1, fromJust gy - 1)
    mul = [lx' * ly', lx' * gy', gx' * gy', gx' * ly']
    l = (L.minimum mul) - 1
    g = (L.maximum mul) + 1
    e = if g - l == 2 then Just $ l + 1 else Nothing
getBounds (Div _ x y) m
  | isJust ex && isJust ey = intBounds $ constInt (fromJust ex `div` fromJust ey)
  | otherwise = (Nothing, Nothing, Nothing, [])
  where
    (_, ex, _, _) = getBounds x m
    (_, ey, _, _) = getBounds y m

getBounds _ _ = (Nothing, Nothing, Nothing, [])


add :: Maybe Int -> Maybe Int -> Maybe Int
add (Just x) (Just y) = Just $ x + y
add _ _ = Nothing

sub :: Maybe Int -> Maybe Int -> Maybe Int
sub (Just x) (Just y) = Just $ x - y
sub _ _ = Nothing

-- | Adjusts bounds for the left hand side of a "less than" expression
ltBounds :: Bounds -> Bounds -> Maybe Bounds
ltBounds (lx, ex, gx, nx) (ly, ey, gy, _) = if checkEq && legal bounds then Just bounds else Nothing
  where
    checkEq = isNothing ex || isNothing ey || ex < ey
    --l' = max' lx (sub ly (Just 1))
    l' = lx
    g' = if isJust ey then min' gx ey else min' gx (sub gy (Just 1))
    e' = if sub l' g' == Just 2 then add l' (Just 1) else ex
    bounds = (l', e', g', nx)

-- | Adjusts bounds for the left hand side of a "greater than" expression
gtBounds :: Bounds -> Bounds -> Maybe Bounds
gtBounds (lx, ex, gx, nx) (ly, ey, gy, _) = if checkEq && legal bounds then Just bounds else Nothing
  where
    checkEq = isNothing ex || isNothing ey || ex > ey
    l' = if isJust ey then max' lx ey else max' lx (add ly (Just 1))
    --g' = min' gx (add gy (Just 1))
    g' = gx
    e' = if sub l' g' == Just 2 then add l' (Just 1) else ex
    bounds = (l', e', g', nx)

-- | Constructs common bounds for an "equal" expression
eqBounds :: Bounds -> Bounds -> Maybe Bounds
eqBounds (lx, ex, gx, nx) (ly, ey, gy, ny) = if checkEq && legal bounds then Just bounds else Nothing
  where
    checkEq = isNothing ex || isNothing ey || ex == ey
    bounds = (max' lx ly, max' ex ey, min' gx gy, nx `L.union` ny)

-- | Adjusts bounds for the left hand side of a "not equal" expression
neqBounds :: Bounds -> Bounds -> Maybe Bounds
neqBounds (lx, ex, gx, nx) (_, ey, _, _) = if checkEq && legal bounds then Just bounds else Nothing
  where
    checkEq = isNothing ex || isNothing ey || ex /= ey
    bounds = (lx, ex, gx, if isJust ey then nx `L.union` [fromJust ey] else nx)

-- | Adjusts bounds for the left hand side of a "less than or equal" expression
leBounds :: Bounds -> Bounds -> Maybe Bounds
leBounds x (l, e, g, _) = ltBounds x (add l (Just 1), add e (Just 1), add g (Just 1), [])

-- | Adjusts bounds for the left hand side of a "greater than or equal" expression
geBounds :: Bounds -> Bounds -> Maybe Bounds
geBounds x (l, e, g, _) = gtBounds x (sub l (Just 1), sub e (Just 1), sub g (Just 1), [])

-- | Constructs bounds for a constant integer for arithmetic calculations
intBounds :: Expr -> Bounds
intBounds (ConstInt _ i) = (Just $ i - 1, Just i, Just $ i + 1, [])
intBounds e = intBounds $ simplifyConst e

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

    f (Mod _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x `mod` y

    f (Add _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x + y
    f (Sub _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x - y
    f (Mul _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x * y
    f (Div _ (ConstInt _ x) (ConstInt _ y)) = Just . constFloat $ (fromIntegral x) / (fromIntegral y)
    f (Pow _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x ^ y

    f (Add _ (ConstInt _ x) (ConstFloat _ y)) = Just . constFloat $ (fromIntegral x) + y
    f (Sub _ (ConstInt _ x) (ConstFloat _ y)) = Just . constFloat $ (fromIntegral x) - y
    f (Mul _ (ConstInt _ x) (ConstFloat _ y)) = Just . constFloat $ (fromIntegral x) * y
    f (Div _ (ConstInt _ x) (ConstFloat _ y)) = Just . constFloat $ (fromIntegral x) / y
    f (Pow _ (ConstInt _ x) (ConstFloat _ y)) = Just . constFloat $ (fromIntegral x) ** y

    f (Add _ (ConstFloat _ x) (ConstInt _ y)) = Just . constFloat $ x + (fromIntegral y)
    f (Sub _ (ConstFloat _ x) (ConstInt _ y)) = Just . constFloat $ x - (fromIntegral y)
    f (Mul _ (ConstFloat _ x) (ConstInt _ y)) = Just . constFloat $ x * (fromIntegral y)
    f (Div _ (ConstFloat _ x) (ConstInt _ y)) = Just . constFloat $ x / (fromIntegral y)
    f (Pow _ (ConstFloat _ x) (ConstInt _ y)) = Just . constFloat $ x ^ y

    f (Add _ (ConstFloat _ x) (ConstFloat _ y)) = Just . constFloat $ x + y
    f (Sub _ (ConstFloat _ x) (ConstFloat _ y)) = Just . constFloat $ x - y
    f (Mul _ (ConstFloat _ x) (ConstFloat _ y)) = Just . constFloat $ x * y
    f (Div _ (ConstFloat _ x) (ConstFloat _ y)) = Just . constFloat $ x / y
    f (Div _ (ConstFloat _ x) (ConstFloat _ y)) = Just . constFloat $ x ** y

    f (Sqrt _ (ConstInt _ x)) = Just . constFloat $ sqrt (fromIntegral x)
    f (Sqrt _ (ConstFloat _ x)) = Just . constFloat $ sqrt x

    --f (Min _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x `min` y
    --f (Max _ (ConstInt _ x) (ConstInt _ y)) = Just . constInt $ x `max` y

    f (Lt _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x < y
    f (Gt _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x > y
    f (Le _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x <= y
    f (Ge _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x >= y
    f (Eq _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x == y
    f (Neq _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x /= y

    f (Lt _ (ConstInt _ x) (ConstFloat _ y)) = Just . constBool $ (fromIntegral x) < y
    f (Gt _ (ConstInt _ x) (ConstFloat _ y)) = Just . constBool $ (fromIntegral x) > y
    f (Le _ (ConstInt _ x) (ConstFloat _ y)) = Just . constBool $ (fromIntegral x) <= y
    f (Ge _ (ConstInt _ x) (ConstFloat _ y)) = Just . constBool $ (fromIntegral x) >= y
    f (Eq _ (ConstInt _ x) (ConstFloat _ y)) = Just . constBool $ (fromIntegral x) == y
    f (Neq _ (ConstInt _ x) (ConstFloat _ y)) = Just . constBool $ (fromIntegral x) /= y

    f (Lt _ (ConstFloat _ x) (ConstInt _ y)) = Just . constBool $ x < (fromIntegral y)
    f (Gt _ (ConstFloat _ x) (ConstInt _ y)) = Just . constBool $ x > (fromIntegral y)
    f (Le _ (ConstFloat _ x) (ConstInt _ y)) = Just . constBool $ x <= (fromIntegral y)
    f (Ge _ (ConstFloat _ x) (ConstInt _ y)) = Just . constBool $ x >= (fromIntegral y)
    f (Eq _ (ConstFloat _ x) (ConstInt _ y)) = Just . constBool $ x == (fromIntegral y)
    f (Neq _ (ConstFloat _ x) (ConstInt _ y)) = Just . constBool $ x /= (fromIntegral y)

    f (Lt _ (ConstFloat _ x) (ConstFloat _ y)) = Just . constBool $ x < y
    f (Gt _ (ConstFloat _ x) (ConstFloat _ y)) = Just . constBool $ x > y
    f (Le _ (ConstFloat _ x) (ConstFloat _ y)) = Just . constBool $ x <= y
    f (Ge _ (ConstFloat _ x) (ConstFloat _ y)) = Just . constBool $ x >= y
    f (Eq _ (ConstFloat _ x) (ConstFloat _ y)) = Just . constBool $ x == y
    f (Neq _ (ConstFloat _ x) (ConstFloat _ y)) = Just . constBool $ x /= y


    f (Eq _ (ConstStr _ x) (ConstStr _ y)) = Just . constBool $ x == y
    f (Eq _ (ConstBool _ x) (ConstBool _ y)) = Just . constBool $ x == y
    f (Eq _ (Var _ x) (Var _ y))
      | x == y    = Just $ constBool True
      | otherwise = Nothing

    f (Neq _ (ConstStr _ x) (ConstStr _ y)) = Just . constBool $ x /= y
    f (Neq _ (ConstBool _ x) (ConstBool _ y)) = Just . constBool $ x /= y

    f (Is _ (ConstInt _ x) (ConstInt _ y)) = Just . constBool $ x == y
    f _ = Nothing
