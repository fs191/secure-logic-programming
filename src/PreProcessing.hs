module PreProcessing 
  ( preProcess
  ) where

import Control.Lens
import Control.Monad.State

import Data.Generics.Uniplate.Data as U

import Annotation
import DatalogProgram
import Rule
import Expr

preProcess :: DatalogProgram -> DatalogProgram
preProcess dp =
  dp & dpRules . traversed %~ simplify
     & dpRules . traversed %~ simplifyRuleHead

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
    f (Add _ x@(ConstInt _ _) y@(ConstInt _ _)) = Just $ binarySimplify (+) x y
    f (Sub _ x@(ConstInt _ _) y@(ConstInt _ _)) = Just $ binarySimplify (-) x y
    f (Mul _ x@(ConstInt _ _) y@(ConstInt _ _)) = Just $ binarySimplify (*) x y
    f (Lt _ x@(ConstInt _ _) y@(ConstInt _ _)) = Just $ binarySimplifyBool (<) x y
    f (Gt _ x@(ConstInt _ _) y@(ConstInt _ _)) = Just $ binarySimplifyBool (>) x y
    f (Le _ x@(ConstInt _ _) y@(ConstInt _ _)) = Just $ binarySimplifyBool (<=) x y
    f (Ge _ x@(ConstInt _ _) y@(ConstInt _ _)) = Just $ binarySimplifyBool (>=) x y
    f (Eq _ x@(ConstInt _ _) y@(ConstInt _ _)) = Just $ binarySimplifyBool (==) x y
    f (Eq _ x@(ConstStr _ _) y@(ConstStr _ _)) = Just $ constBool $ x == y
    f (Eq _ (Var _ x) (Var _ y))
      | x == y    = Just $ constBool True
      | otherwise = Nothing
    f _ = Nothing

binarySimplify :: (Int -> Int -> Int) -> Expr -> Expr -> Expr
binarySimplify f (ConstInt a x) (ConstInt b y) = 
  (\e -> ConstInt e (f x y)) (unifyAnns a b)

binarySimplifyBool :: (Int -> Int -> Bool) -> Expr -> Expr -> Expr
binarySimplifyBool f (ConstInt a x) (ConstInt b y) = 
  (\e -> ConstBool e (f x y)) (unifyAnns a b)

-- | Ensures that the rule head only consists of variables
simplifyRuleHead :: Rule -> Rule
simplifyRuleHead r = r & ruleHead . predArgs .~ _newArgs
                       & ruleTail %~ (\x -> foldr eAnd x _stmts)
                       & id %~ refreshRule "X"
  where
    _args = r ^. ruleHead . predArgs
    _nonVars = _args ^.. folded . filtered (isn't _Var)
    _stmts = evalState (traverse _genStmt _nonVars) (0 :: Int)
    _newArgs = evalState (traverse _genArgs _args) (0 :: Int)
    _genStmt :: Expr -> State Int Expr
    _genStmt x =
      do
        i <- get
        modify (+1)
        return $ equal (Var (x ^. annotation) $ "$A" ++ show i) x
    _genArgs x@(Var{}) = return x
    _genArgs x =
      do
        i <- get
        modify (+1)
        return $ Var (x ^. annotation) $ "$A" ++ show i

