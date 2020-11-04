{-# LANGUAGE FlexibleContexts #-}
module PreProcessing 
  ( preProcess
  ) where

import Relude

import Control.Lens
import Control.Monad.Except

import Data.Generics.Uniplate.Data as U

import ErrorMsg
import Annotation
import DatalogProgram
import Rule
import Expr

preProcess 
  :: (MonadError CompilerException m) 
  => DatalogProgram 
  -> m DatalogProgram
preProcess dp = 
  do
    dp1 <- dp & dpRules . traversed %%~ simplify
    let dp' = flip evalState (0::Int) $
             dp1 & dpRules . traversed %~ simplifyRuleHead
                 & id %%~ U.transformBiM holeToVar
    return $ dp' & dpRules %~ concatMap ruleNonDetSplit

-- | Rewrites constant terms to simpler terms
simplify 
  :: (MonadError CompilerException m) 
  => Rule 
  -> m Rule
simplify r = r & ruleTail %%~ U.rewriteM f
  where
    f (And _ (ConstBool _ True) x) = return $ Just x
    f (And _ x (ConstBool _ True)) = return $ Just x
    f (And _ (ConstBool _ False) _) = return . Just $ constBool False
    f (And _ _ (ConstBool _ False)) = return . Just $ constBool False
    f (Or _ (ConstBool _ True) _) = return . Just $ constBool True
    f (Or _ _ (ConstBool _ True)) = return . Just $ constBool True
    f (Or _ (ConstBool _ False) x) = return $ Just x
    f (Or _ x (ConstBool _ False)) = return $ Just x
    f (Add _ x@(ConstInt _ _) y@(ConstInt _ _)) = Just <$> binarySimplify (+) x y
    f (Sub _ x@(ConstInt _ _) y@(ConstInt _ _)) = Just <$> binarySimplify (-) x y
    f (Mul _ x@(ConstInt _ _) y@(ConstInt _ _)) = Just <$> binarySimplify (*) x y
    f (Lt _ x@(ConstInt _ _) y@(ConstInt _ _)) = Just <$> binarySimplifyBool (<) x y
    f (Gt _ x@(ConstInt _ _) y@(ConstInt _ _)) = Just <$> binarySimplifyBool (>) x y
    f (Le _ x@(ConstInt _ _) y@(ConstInt _ _)) = Just <$> binarySimplifyBool (<=) x y
    f (Ge _ x@(ConstInt _ _) y@(ConstInt _ _)) = Just <$> binarySimplifyBool (>=) x y
    f (Eq _ x@(ConstInt _ _) y@(ConstInt _ _)) = Just <$> binarySimplifyBool (==) x y
    f (Eq _ x@(ConstStr _ _) y@(ConstStr _ _)) = return . Just $ constBool $ x == y
    f (Eq _ (Var _ x) (Var _ y))
      | x == y    = return . Just $ constBool True
      | otherwise = return Nothing
    f _ = return Nothing

binarySimplify 
  :: (MonadError CompilerException m) 
  => (Int -> Int -> Int) 
  -> Expr 
  -> Expr 
  -> m Expr
binarySimplify f l@(ConstInt a x) r@(ConstInt b y) =
  (\c' -> ConstInt c' $ f x y) <$> c 
  where
    c = maybe err return $ unifyAnns a b
    err = throwError $ UnificationFailed l r
binarySimplify _ _ _ = error "Expecting integers in binarySimplify"

binarySimplifyBool 
  :: (MonadError CompilerException m) 
  => (Int -> Int -> Bool) 
  -> Expr 
  -> Expr 
  -> m Expr
binarySimplifyBool f l@(ConstInt a x) r@(ConstInt b y) = 
  (\c' -> ConstBool c' $ f x y) <$> c 
  where
    c = maybe err return $ unifyAnns a b
    err = throwError $ UnificationFailed l r
binarySimplifyBool _ _ _ = error "Expecting integers in binarySimplifyBool"

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
        let f = if x ^. annotation . annBound
                  then equal
                  else eUn
        return $ f (Var (x ^. annotation) $ "$A" <> show i) x
    _genArgs x@(Var{}) = return x
    _genArgs x =
      do
        i <- get
        modify (+1)
        return $ Var (x ^. annotation) $ "$A" <> show i

holeToVar :: Expr -> State Int Expr
holeToVar (Hole e) =
  do
    n <- get
    modify (+1)
    return . Var e $ "__HOLE__" <> show n
holeToVar x = return x

ruleNonDetSplit :: Rule -> [Rule]
ruleNonDetSplit r = map (Rule rHead) newTails
  where
    rHead   = r ^. ruleHead
    dnfTail = r ^. ruleTail . to toDNF
    newTails = orsToList dnfTail

