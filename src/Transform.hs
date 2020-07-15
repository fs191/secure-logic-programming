{-# LANGUAGE ScopedTypeVariables #-}

module Transform
  ( deriveAllGroundRules
  ) where

---------------------------------------------------------
---- Transformation of a Datalog script
----   to intermediate representation
---------------------------------------------------------

import Data.Generics.Uniplate.Operations as U
import Data.List
import Data.Maybe

import Control.Lens as L
import Control.Monad

import Rule
import Expr
import Substitution
import Annotation
import qualified DatalogProgram as DP

-- | Generates all possible ground rules for n iterations
deriveAllGroundRules :: DP.DatalogProgram -> Int -> Maybe DP.DatalogProgram
deriveAllGroundRules program n = program'
                                   -- & fromMaybe program' . adornProgram
                                   & DP.ruleLens %%~ f
  where
    -- Input program but db clauses are converted to rules
    program' = program
    f :: [Rule] -> Maybe [Rule]
    f = foldl (>=>) return $ replicate n pipeline
    pipeline :: [Rule] -> Maybe [Rule]
    pipeline 
      =   Just . removeDuplicateFacts 
      >=> Just . removeFalseFacts 
      >=> Just . map simplify 
      >=> Just . (traversed . ruleTail %~ simplifyAnds :: [Rule] -> [Rule]) 
      >=> Just . map (refreshRule "X_") 
      >=> (traversed %%~ simplifyVars) 
      >=> inlineOnce

-- | Tries to unify each predicate in each rule body with an appropriate rule
inlineOnce :: [Rule] -> Maybe [Rule]
inlineOnce rs =
  Just rs <> inlined
  where
    inlined = sequenceA $ do
      tgt <- refreshRule "T_" <$> rs
      src <- rs
      let shd = src ^. ruleHead
      let stl = src ^. ruleTail
      let ttl = tgt ^. ruleTail
      (p@Pred {}, mut) <- U.contexts ttl
      let subst = unify shd p
      s <- maybeToList subst
      return . applySubst s $ tgt & ruleTail .~ mut stl

-- | Rewrites constant terms to simpler terms
simplify :: Rule -> Rule
simplify r = r & ruleTail %~ U.rewrite f
  where
    f (And _ (ConstBool _ True) x) = Just x
    f (And _ x (ConstBool _ True)) = Just x
    f (And _ (ConstBool _ False) _) = Just $ constBool False
    f (And _ _ (ConstBool _ False)) = Just $ constBool False
    f (Or _ (ConstBool _ True) _) = Just $ constBool True
    f (Or _ _ (ConstBool _ True)) = Just $ constBool True
    f (Or _ (ConstBool _ False) x) = Just x
    f (Or _ x (ConstBool _ False)) = Just x
    f (Add _ x@(ConstInt _ _) y@(ConstInt _ _)) = binarySimplify (+) x y
    f (Sub _ x@(ConstInt _ _) y@(ConstInt _ _)) = binarySimplify (-) x y
    f (Mul _ x@(ConstInt _ _) y@(ConstInt _ _)) = binarySimplify (*) x y
    f (Min _ x@(ConstInt _ _) y@(ConstInt _ _)) = binarySimplify min x y
    f (Max _ x@(ConstInt _ _) y@(ConstInt _ _)) = binarySimplify max x y
    f (Lt _ x@(ConstInt _ _) y@(ConstInt _ _)) = binarySimplifyBool (<) x y
    f (Gt _ x@(ConstInt _ _) y@(ConstInt _ _)) = binarySimplifyBool (>) x y
    f (Le _ x@(ConstInt _ _) y@(ConstInt _ _)) = binarySimplifyBool (<=) x y
    f (Ge _ x@(ConstInt _ _) y@(ConstInt _ _)) = binarySimplifyBool (>=) x y
    f (Eq _ x@(ConstInt _ _) y@(ConstInt _ _)) = binarySimplifyBool (==) x y
    f (Eq _ x@(ConstStr _ _) y@(ConstStr _ _)) = Just . constBool $ x == y
    f (Eq _ (Var _ x) (Var _ y))
      | x == y    = Just $ constBool True
      | otherwise = Nothing
    f _ = Nothing

binarySimplify :: (Int -> Int -> Int) -> Expr -> Expr -> Maybe Expr
binarySimplify f (ConstInt a x) (ConstInt b y) = 
  (\ann -> ConstInt ann (f x y)) <$> (a <^ b)

binarySimplifyBool :: (Int -> Int -> Bool) -> Expr -> Expr -> Maybe Expr
binarySimplifyBool f (ConstInt a x) (ConstInt b y) = 
  (\ann -> ConstBool ann (f x y)) <$> (a <^ b)

simplifyVars :: Rule -> Maybe Rule
simplifyVars r = applySubst subst r
  where subst = simplifyVars' $ r ^. ruleTail

-- | Removes any unnecessary variable equalities (e.g. X=Y)
simplifyVars' :: Expr -> Subst
simplifyVars' r = compress . mconcat . catMaybes $ f <$> U.universe r
  where f (Eq _ v x)
          | isLeaf x  = v |-> x
          | otherwise = Nothing
        f (Eq _ x v)
          | isLeaf x  = v |-> x
          | otherwise = Nothing
        f _ = Nothing

-- Removes duplicate terms from AND operations at the root expression
-- TODO: find out what's causing it to do weird substitutions in the market.pl example
simplifyAnds :: Expr -> Expr
simplifyAnds x = foldr1 eAnd . nub . filter (not . isAnd) $ simplifyAnds' x

simplifyAnds' :: Expr -> [Expr]
simplifyAnds' (And _ x y) = simplifyAnds' x <> simplifyAnds' y
simplifyAnds' x = [x]

isAnd :: Expr -> Bool
isAnd And {} = True
isAnd _ = False

-- | Removes facts that always evaluate to False
removeFalseFacts :: [Rule] -> [Rule]
removeFalseFacts = filter (\x -> x ^. ruleTail /= constBool False)

-- | Removes duplicates of facts that appear more than once
-- TODO make it preserve the order of predicates
removeDuplicateFacts :: [Rule] -> [Rule]
removeDuplicateFacts = nub

-- | Binds constants that are arguments of some predicate to a new variable
--bindArgColumns :: Expr -> State Int Expr
--bindArgColumns (Pred ann n as) =
--  do
--    let freshVar :: State Int Expr
--        freshVar =
--          do
--            i <- get
--            put $ i + 1
--            return . var $ "_CONST_" <> show i
--    let cols = [x | x@(ConstStr _ _) <- as]
--    vars <- sequenceA $ replicate (length cols) freshVar
--    let cvs   = cols `zip` vars
--        lkp :: Expr -> Expr
--        lkp x = fromMaybe x $ lookup x cvs
--        newAs = lkp <$> as
--        eqs   = uncurry equal <$> cvs
--        newPred = Pred ann n newAs
--    return $ foldr eAnd newPred eqs
--bindArgColumns x = return x

