{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Substitution
  ( Subst(..)
  , substFromMap
  , refreshExpr
  , applyToExpr
  , refreshAndApply
  , emptyTheta
  , unify
  , compress
  , (|->)
  , keys, elems
  , getValue
  ) where

---------------------------------------------------------
---- Substitution map and related operations
---------------------------------------------------------

import Relude

import qualified Data.Map as M

import Data.Generics.Uniplate.Data
import Data.List (nub)
import Data.Text.Prettyprint.Doc
import qualified Data.Text as T

import Control.Exception (assert)
import Control.Lens hiding (universe, transform, transformM)
import Control.Lens.Extras
import Control.Monad.Trans.UnionFind

import qualified Text.Show

import Expr
import ExprPretty
import Annotation

-- | Representation of substitution on variables. Only works on
-- expressions that have an identifier. Currently does not preserve types.
newtype Subst = Th (M.Map Text Expr)
  deriving (Semigroup, Monoid, Eq)

instance Show Subst where
  show (Th theta) = concat $ map (\(k,v) -> show k ++ " -> " ++ show v ++ "\n") (M.toList theta)

instance Pretty Subst where
  pretty (Th s) = tupled $ (\(a, b) -> pretty a <+> "->" <+> prettyFull b) <$> M.toList s

-- | A very safe string that is prepended to variable names to keep track
-- of which variables have already been substituted. Only used internally.
safeStr :: Text
safeStr = "$!!?"

substFromMap :: M.Map Text Expr -> Subst
substFromMap = Th

-- | Compresses the substitution using union-find. 
--
-- @
--   compress $ mconcat [Var "x" |-> Var "y", Var "y" |-> Var "z"] ==
--              mconcat [Var "x" |-> Var "z", Var "y" |-> Var "z"]
-- @
compress :: Subst -> Subst
compress (Th m) = runIdentity . runUnionFind $
  do
    let joins = [(k, v) | (k, Var _ v) <- M.toList m]
        vs = nub $ (M.keys m) <> [v | Var _ v <- M.elems m]
    points <- traverse fresh vs
    let pointMap :: M.Map Text (Point Text)
        pointMap = M.fromList $ vs `zip` points
        joins' = [(pointMap M.! x, pointMap M.! y) | (x,y) <- joins]
    traverse_ (uncurry union) joins'
    reprMap <- traverse repr pointMap 
    -- Map from initial var names to representative var names
    descMap <- traverse descriptor reprMap
    let varByName :: Text -> Maybe Expr
        varByName n = m ^? to M.elems 
                         . folded 
                         . filtered (\x -> fromMaybe False $ x ^? _Var . _2 . to (==n))
    let mapper :: Expr -> Expr
        mapper v@(Var _ n) = fromMaybe v $
          do
            n' <- M.lookup n descMap
            M.lookup n' m <|> varByName n'
        mapper x = x
    return . Th $ M.map mapper m

-- | Unit substitution
emptyTheta :: Subst
emptyTheta = Th $ M.empty

-- | Substitute variable `x` with term `y`
(|->) :: Expr -> Expr -> Subst
v |-> y = Th (M.singleton x t)
  where x = fromMaybe err  $ identifier v
        t = fromMaybe err2 $ unifyExprAnns y v
        err =  error $ show v <> "does not have an identifier"
        err2 = error $ "Failed to unify " <> show v <> " and " <> show y

-- | Get the substitution term for variable `x`
evalTheta :: Subst -> Expr -> Expr
evalTheta (Th theta) v@(Var a n) = 
  if M.member n theta 
    then 
      fromMaybe err $ x & annotation %%~ (unifyAnns a)
    else v
  where 
    x = theta M.! n
    err = error $ "Failed to unify " <> show x <> " and " <> show a
evalTheta _ x = error $ "Attempting to unify a non-variable: " <> show x

-- | Apply a substitution to an expression
-- TODO: unify type and domain as well
applyToExpr :: Subst -> Expr -> Expr
applyToExpr theta bexpr = removeSafePrefixes . transform f $ safePrefix _bexpr
  where theta'      = mapKeys (safeStr<>) theta
        f v@(Var _ _) = evalTheta theta' v
        f x         = x
        _vars       = [b | (Var _ b) <- universe bexpr]
        -- Ensure that no variables begin with the safe string before application
        _bexpr      = assert (all (not . T.isPrefixOf safeStr) _vars) bexpr

removeSafePrefixes :: Expr -> Expr
removeSafePrefixes = transform f
  where
    f (Var a x) = Var a $ fromMaybe x $ T.stripPrefix safeStr x
    f x = x

mapKeys :: (Text -> Text) -> Subst -> Subst
mapKeys f (Th theta) = Th $ M.mapKeys f theta

-- | Rename all the variables in `e` by 
-- enumerating them and prepending the names with `prefix`
refreshExpr :: Text -> Expr -> Subst
refreshExpr prefix e = mconcat $ evalState substs (0 :: Int)
  where 
    substs = traverse f $ nub [v | v@(Var _ _) <- universe e]
    f v@(Var a _) =
      do
        i <- get
        modify (+1)
        let n = prefix <> show i
        return $ v |-> Var a n
    f _ = error "Expected a variable, got something else"

-- | Prefixes a variable name with the safe string to keep track which variables
-- have already been substituted
safePrefix :: Expr -> Expr
safePrefix = transform f
  where f (Var a n) = Var a (safeStr <> n)
        f x = x

-- | Immediately refreshes variable names in `e`
refreshAndApply :: Text -> Expr -> Expr
refreshAndApply prefix e = applyToExpr (refreshExpr prefix e) e

-- | Attempt to unify the two expressions. Will return Nothing if the expressions cannot be unified
unify :: Expr -> Expr -> Maybe Subst
unify x y = unify' [(x, y)]

-- See: https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm
unify' :: [(Expr, Expr)] -> Maybe Subst
unify' [] = Just emptyTheta
-- Eliminate
unify' g@((v@(Var{}), y):t)
  | v `elem` vars g = 
    do
      let subst = v |-> y
      let s = applyToExpr subst
      rest <- unify' (t & traversed . both %~ s)
      return $ subst <> rest
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
unify' ((Pow _ x1 x2, Pow _ y1 y2):t) = unify' $ [x1,x2] `zip` [y1,y2] <> t
--unify' ((Min _ x1 x2, Min _ y1 y2):t) = unify' $ [x1,x2] `zip` [y1,y2] <> t
--unify' ((Max _ x1 x2, Max _ y1 y2):t) = unify' $ [x1,x2] `zip` [y1,y2] <> t
unify' ((Not _ x, Not _ y):t) = unify' $ [x] `zip` [y] <> t
unify' ((Neg _ x, Neg _ y):t) = unify' $ [x] `zip` [y] <> t
unify' ((Inv _ x, Inv _ y):t) = unify' $ [x] `zip` [y] <> t
unify' ((Sqrt _ x, Sqrt _ y):t) = unify' $ [x] `zip` [y] <> t
unify' ((Query _ i _, Query _ j _):t)
  | i == j    = unify' t
  | otherwise = Nothing
-- Swap
unify' ((x, y@(Var{})):t) = unify' $ (y, x):t
-- Delete / conflict
unify' ((x,y):t) 
  | x `valEq` y    = unify' t
  | otherwise = Nothing

valEq :: Expr -> Expr -> Bool
valEq (ConstInt _ x) (ConstInt _ y)   = x == y
valEq (ConstStr _ x) (ConstStr _ y)   = x == y
valEq (ConstBool _ x) (ConstBool _ y) = x == y
valEq (Var _ x) (Var _ y)             = x == y
valEq _ _                             = False

--
-- Utils
--

vars :: [(Expr, Expr)] -> [Expr]
vars x = filter (is _Var) . uncurry (<>) $ unzip x

keys :: Subst -> [Expr]
keys (Th theta) = map var (M.keys theta)

elems :: Subst -> [Expr]
elems (Th theta) = M.elems theta
getValue (Th theta) = (theta M.!)
