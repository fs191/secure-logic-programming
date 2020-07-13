{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module TypeInference
  ( typeInference
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import Control.Exception

import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Generics.Uniplate.Data as U

import Annotation
import Expr
import Rule
import DatalogProgram as DP
import Language.SecreC.Types

data InferenceException
  = TypeMismatch String
  | RuleNotFound String
  deriving (Show, Exception)

data InferenceState = InferenceState
  { _istProg  :: DatalogProgram
  }

type InferenceM = State InferenceState
data TypeSubstitution = TypeSubstitution (Map String PPDomain)

makeLenses ''InferenceState

instance Semigroup TypeSubstitution where
  (TypeSubstitution x) <> (TypeSubstitution y) = 
    TypeSubstitution $ M.unionWith unifyDomains x y

instance Monoid TypeSubstitution where
  mempty = TypeSubstitution mempty

applyTypeSubst :: TypeSubstitution -> Rule -> Rule
applyTypeSubst (TypeSubstitution ts) r = 
  r & ruleHead %~ f
    & ruleTail %~ f
  where
    t v@(Var _ n) = fromMaybe (exprTyping v) $ M.lookup n ts
    f v = applyTyping (t v) v
applyTypeSubst (TypeSubstitution _) x = x

(|->) :: String -> PPDomain -> TypeSubstitution
(|->) x y = TypeSubstitution $ M.singleton x y

evalInferenceM :: InferenceM a -> DatalogProgram -> a
evalInferenceM x dp = evalState x (defaultState dp)

defaultState :: DatalogProgram -> InferenceState
defaultState dp = InferenceState dp

-- Assuming all rules are ground rules and all rule heads are annotated with
-- binding patterns
typeInference :: DatalogProgram -> DatalogProgram
typeInference = evalInferenceM act
  where
    act = do
      _dp <- use istProg
      _dp & dpRules . traversed %%~ inferRule

-- | Infers types and domains from database facts
inferRule :: Rule -> InferenceM Rule
inferRule r =
  do
    _scope <- traverse inferFromDB . U.universe $ r ^. ruleTail
    let app v = applyTypeSubst (mconcat _scope) v
        _applied  = U.transformBi app r
    return _applied

inferFromDB :: Expr -> InferenceM TypeSubstitution
inferFromDB (Pred _ n xs) =
  do
    dp <- use istProg
    -- Get the database fact corresponding to the predicate
    let ext :: [Rule]
        ext = extensionalFacts dp
        matches = L.filter (\x -> ruleName x == n) ext
        _args = case L.null matches of
          True  -> []
          False -> head matches ^. ruleHead . _Pred . _3 
    -- Unify the parameter typings in the database fact and the predicate
    let unified = (uncurry unifyExprTypes) <$> (xs `zip` _args)
    -- Take bound and unbound variables into account
    let inferParams :: (Expr, PPDomain) -> TypeSubstitution
        inferParams (v@(Var e n'), x) 
          | e ^. annBound = n' |-> exprTyping v
          | otherwise     = n' |-> x
        newParams' = inferParams <$> (xs `zip` unified)
    -- Unify the new type substitutions with existing substitutions
    return $ mconcat newParams'
inferFromDB _ = return mempty

inferFromGoal :: Expr -> [Rule] -> InferenceM TypeSubstitution
inferFromGoal g rs =
  do
    undefined

inferFromExpr :: InferenceM [Rule]
inferFromExpr = undefined

applyTyping :: PPDomain -> Expr -> Expr
applyTyping d e = e & annLens . domain  %~  unifyDomains d

unifyExprTypes :: Expr -> Expr -> PPDomain
unifyExprTypes x y = unifyDomains xd yd
  where
    xd = head $ x ^.. annLens . domain
    yd = head $ y ^.. annLens . domain


exprTyping :: Expr -> PPDomain
exprTyping e = e ^. annLens . domain

