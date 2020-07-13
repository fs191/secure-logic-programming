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
  , _istScope :: [Map String Typing]
  }

makeLenses ''Typing
makeLenses ''InferenceState

type InferenceM = StateT InferenceState (Except InferenceException)

evalInferenceM :: InferenceM a -> DatalogProgram -> Either InferenceException a
evalInferenceM x dp = runExcept $ evalStateT x (defaultState dp)

defaultState :: DatalogProgram -> InferenceState
defaultState dp = InferenceState dp [M.empty]

-- Assuming all rules are ground rules and all rule heads are annotated with
-- binding patterns
typeInference :: DatalogProgram -> Either InferenceException DatalogProgram
typeInference = evalInferenceM act
  where
    act = do
      _dp <- use istProg
      _dp & dpRules . traversed %%~ inferRule

inferRule :: Rule -> InferenceM Rule
inferRule r =
  do
    void . traverse f . U.universe $ r ^. ruleTail
    _scope <- use $ istScope . _head
    let app v@(Var _ n) = applyTyping appf v
          where appf = fromMaybe (exprTyping v) $ M.lookup n _scope
        app x           = Right x
        _applied  = U.transformBiM app r
    case _applied of
      Right x -> return x
      Left x  -> throwError $ TypeMismatch x
  where f :: Expr -> InferenceM ()
        f (Pred _ n xs) =
          do
            dp <- use istProg
            let ext :: [Rule]
                ext = extensionalFacts dp
                matches = L.filter (\x -> ruleName x == n) ext
            let _args = case L.null matches of
                  True  -> []
                  False -> head matches ^. ruleHead . _Pred . _3 
            let zipped = xs `zip` _args
            unified <- case traverse (uncurry unifyExprTypes) zipped of
                              Just x  -> return x
                              Nothing -> throwError $ TypeMismatch ""
            let newParams  = xs `zip` unified
                newParams' = fromMaybe (throw $ TypeMismatch "") $ traverse inferParams newParams
                unifier :: Typing -> Typing -> Typing
                unifier a b = fromMaybe (throw $ TypeMismatch "") $ unifyTypings a b
            istScope . _head %= M.unionWith unifier (M.fromList newParams')
            return ()
        f _ = return ()

inferParams :: (Expr, Typing) -> Maybe (String, Typing)
inferParams (v@(Var e n), x@(Typing _ t)) 
  | e ^. annBound = (n, exprTyping v) & _2 . tType %%~ unifyTypes t
  | otherwise     = Just (n, x)

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

