{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module PrivacyInference
  ( privacyInference
  ) where

import Control.Lens
import Control.Monad.State

import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Generics.Uniplate.Data as U

import Annotation
import Expr
import Rule
import DatalogProgram as DP
import Language.SecreC.Types

data InferenceState = InferenceState
  { _istProg  :: DatalogProgram
  }

type InferenceM = State InferenceState

-- TODO replace this with regular substitution once it can properly unify
-- types
data TypeSubstitution = TypeSubstitution (Map String PPDomain)
  deriving (Show)

makeLenses ''InferenceState

instance Semigroup TypeSubstitution where
  (TypeSubstitution x) <> (TypeSubstitution y) = 
    TypeSubstitution $ M.unionWith unifyDomains x y

instance Monoid TypeSubstitution where
  mempty = TypeSubstitution mempty

applyTypeSubst :: TypeSubstitution -> Rule -> Rule
applyTypeSubst ts r = 
  r & ruleHead %~ applyTypeSubstToExpr ts
    & ruleTail %~ applyTypeSubstToExpr ts

applyTypeSubstToExpr :: TypeSubstitution -> Expr -> Expr
applyTypeSubstToExpr (TypeSubstitution ts) = U.transform f
  where
    t v = fromMaybe (exprTyping v) $ identifier v >>= (`M.lookup` ts)
    f v = applyTyping (t v) v

(|->) :: String -> PPDomain -> TypeSubstitution
(|->) x y = TypeSubstitution $ M.singleton x y

evalInferenceM :: InferenceM a -> DatalogProgram -> a
evalInferenceM x dp = evalState x (defaultState dp)

defaultState :: DatalogProgram -> InferenceState
defaultState dp = InferenceState dp

-- Assuming all rules are ground rules and all rule heads are annotated with
-- binding patterns
privacyInference :: DatalogProgram -> DatalogProgram
privacyInference = evalInferenceM act
  where
    act = do
      _prog <- use istProg
      _p <- _prog & dpRules . traversed %%~ 
        (inferRule >=> inferFromGoalAction)
      return $ inferGoal _p
    inferFromGoalAction :: Rule -> InferenceM Rule
    inferFromGoalAction x = do
      s <- inferFromGoal x
      return $ applyTypeSubst s x

-- | Infers types and domains from database facts
inferRule :: Rule -> InferenceM Rule
inferRule r =
  do
    _scope <- traverse inferFromDB . U.universe $ r ^. ruleTail
    return $ applyTypeSubst (mconcat _scope) r

inferFromDB :: Expr -> InferenceM TypeSubstitution
inferFromDB (Pred _ n xs) =
  do
    dp <- use istProg
    -- Get the database fact corresponding to the predicate
    let ext :: [Rule]
        ext = extensionalFacts dp
        matches = L.filter (\x -> ruleName x == n) ext
        _dbargs = matches ^. _head . ruleHead . _Pred . _3 
    -- Unify the argument typings in the database fact and the predicate
    let unified :: [PPDomain]
        unified = (uncurry unifyExprDomains) <$> (xs `zip` _dbargs)
    -- Take bound and unbound variables into account
    let inferParams :: (Expr, PPDomain) -> TypeSubstitution
        inferParams (v, x) 
          | v ^. annLens . annBound = mempty
          | otherwise     = (fromMaybe undefined $ identifier v) |-> x
        newParams' = inferParams <$> (xs `zip` unified)
    -- Unify the new type substitutions with existing substitutions
    return $ mconcat newParams'
inferFromDB _ = return mempty

inferFromGoal :: Rule -> InferenceM TypeSubstitution
inferFromGoal r =
  do
    g <- use $ istProg . dpGoal
    let subst =
          do
            rn <- r ^? ruleHead . _Pred . _2
            gn <- g ^? _Pred . _2
            guard $ rn == gn
            rxs <- r ^? ruleHead . _Pred . _3 :: Maybe [Expr]
            gxs <- g ^? _Pred . _3 :: Maybe [Expr]
            let unified = (uncurry unifyExprDomains) <$> (rxs `zip` gxs)
                f (x, y) = (,) <$> (maybeToList $ identifier x) <*> [y]
                paramNames = concat $ traverse f $ rxs `zip` unified
            return . mconcat $ (uncurry (|->)) <$> paramNames
    return $ fromMaybe mempty subst

inferGoal :: DatalogProgram -> DatalogProgram
inferGoal dp = dp & dpGoal  %~ f
                  & outputs %~ f
                  & inputs  %~ f
  where 
    gxs = dp ^.. dpGoal . _Pred . _3 . folded . to identifier
    n = dp ^. dpGoal . _Pred . _2
    goalRules = zip gxs . transpose $ dp ^.. dpRules 
                  . folded 
                  . filtered (\x -> ruleName x == n) 
                  . ruleHead 
                  . _Pred 
                  . _3
    foldUnify x = foldl1 unifyDomains $ x ^.. folded . annLens . domain
    f = applyTypeSubstToExpr $ 
          mconcat [x |-> foldUnify y | (Just x, y) <- goalRules]

applyTyping :: PPDomain -> Expr -> Expr
applyTyping d e = e & annLens . domain  %~  unifyDomains d

exprTyping :: Expr -> PPDomain
exprTyping e = e ^. annLens . domain

