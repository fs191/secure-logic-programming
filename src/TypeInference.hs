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
import Data.Text.Prettyprint.Doc

import Annotation
import Expr
import Rule
import DatalogProgram as DP
import Language.SecreC.Types ()

data InferenceException
  = TypeMismatch String
  | RuleNotFound String
  deriving (Show, Exception)

data Typing = Typing 
  { _tDom  :: PPDomain 
  , _tType :: PPType
  }

data InferenceState = InferenceState
  { _istProg  :: DatalogProgram
  }

type InferenceM = StateT InferenceState (Except InferenceException)
data TypeSubstitution 
  = TypeSubstitution (Map String Typing)
  | SubstitutionFail

makeLenses ''Typing
makeLenses ''InferenceState

instance Semigroup TypeSubstitution where
  SubstitutionFail <> _ = SubstitutionFail
  _ <> SubstitutionFail = SubstitutionFail
  (TypeSubstitution x) <> (TypeSubstitution y) = 
    TypeSubstitution $ M.unionWith unifier x y
    where
      unifier :: Typing -> Typing -> Typing
      unifier a b = fromMaybe (throw $ TypeMismatch "") $ unifyTypings a b

instance Monoid TypeSubstitution where
  mempty = TypeSubstitution mempty

applyTypeSubst :: TypeSubstitution -> Rule -> Either String Rule
applyTypeSubst (TypeSubstitution ts) r = 
  do
    r' <- r & ruleHead %%~ f
    r' & ruleTail %%~ f
  where
    t v@(Var _ n) = fromMaybe (exprTyping v) $ M.lookup n ts
    f v = applyTyping (t v) v
applyTypeSubst (TypeSubstitution _) x = return x

(|->) :: String -> Typing -> TypeSubstitution
(|->) x y = TypeSubstitution $ M.singleton x y

evalInferenceM :: InferenceM a -> DatalogProgram -> Either InferenceException a
evalInferenceM x dp = runExcept $ evalStateT x (defaultState dp)

defaultState :: DatalogProgram -> InferenceState
defaultState dp = InferenceState dp

-- Assuming all rules are ground rules and all rule heads are annotated with
-- binding patterns
typeInference :: DatalogProgram -> Either InferenceException DatalogProgram
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
        _applied  = U.transformBiM app r
    case _applied of
      Right x -> return x
      Left x  -> throwError $ TypeMismatch x

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
    let zipped = xs `zip` _args
    unified <- case traverse (uncurry unifyExprTypes) zipped of
                      Just x  -> return x
                      Nothing -> throwError $ TypeMismatch ""
    -- Take bound and unbound variables into account
    let inferParams :: (Expr, Typing) -> Maybe TypeSubstitution
        inferParams (v@(Var e n'), x@(Typing _ t)) 
          | e ^. annBound = (n' |->) <$> (exprTyping v & tType %%~ unifyTypes t)
          | otherwise     = Just $ n' |-> x
        newParams  = xs `zip` unified
        newParams' = traverse inferParams newParams
    -- Unify the new type substitutions with existing substitutions
    return (mconcat $ fromMaybe (throw $ TypeMismatch "") newParams')
inferFromDB _ = return mempty

inferFromGoal :: Expr -> [Rule] -> InferenceM TypeSubstitution
inferFromGoal g rs =
  do
    undefined

inferFromExpr :: InferenceM [Rule]
inferFromExpr = undefined

applyTyping :: Typing -> Expr -> Either String Expr
applyTyping (Typing d t) e =
  case res of
    Just x  -> Right x
    Nothing -> Left . show $ pretty e
  where
    res = e & annLens . domain  %~  unifyDomains d
            & annLens . annType %%~ unifyTypes t

unifyTypings :: Typing -> Typing -> Maybe Typing
unifyTypings (Typing xd xt) (Typing yd yt) = Typing ud <$> ut
  where ud = unifyDomains xd yd
        ut = unifyTypes xt yt

unifyExprTypes :: Expr -> Expr -> Maybe Typing
unifyExprTypes x y =
  do
    let xt = head $ x ^.. annLens . annType
        yt = head $ y ^.. annLens . annType
        xd = head $ x ^.. annLens . domain
        yd = head $ y ^.. annLens . domain
    tu <- unifyTypes xt yt
    let du   = unifyDomains xd yd
    return $ Typing du tu

unifyTypes :: PPType -> PPType -> Maybe PPType
unifyTypes x y
  | x == PPAuto = Just y
  | y == PPAuto || x == y
                = Just x
  | otherwise   = Nothing

unifyDomains :: PPDomain -> PPDomain -> PPDomain
unifyDomains Public Public = Public
unifyDomains Unknown x     = x
unifyDomains x Unknown     = x
unifyDomains _ _           = Private

exprTyping :: Expr -> Typing
exprTyping e = Typing d t
  where d = e ^. annLens . domain
        t = e ^. annLens . annType

