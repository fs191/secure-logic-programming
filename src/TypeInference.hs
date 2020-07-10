{-# LANGUAGE TemplateHaskell #-}

module TypeInference
  ( typeInference
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Except

import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Generics.Uniplate.Data as U

import Expr
import Rule
import DatalogProgram as DP
import Language.SecreC.Types ()

data InferenceException
  = TypeMismatch

data Typing = Typing PPDomain PPType

data InferenceState = InferenceState
  { _istProg  :: DatalogProgram
  , _istScope :: [Map String Typing]
  }
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
    let _applied  = U.transformBiM (\v@(Var _ n) -> applyTyping (_scope M.! n) v) r
    when (isNothing _applied) $ throwError TypeMismatch
    undefined
  where f :: Expr -> InferenceM ()
        f (Pred _ n xs) =
          do
            dp <- use istProg
            let ext :: [Rule]
                ext = extensionalFacts dp
                matches = head (L.filter (\x -> ruleName x == n) ext) ^. ruleHead . _Pred . _3
                zipped = xs `zip` matches
                unified = sequence $ uncurry unifyExprTypes <$> zipped
            when (isNothing unified) $ throwError TypeMismatch
            let newParams = xs `zip` fromJust unified
                newParams' = [(n', x) | (Var _ n', x) <- newParams]
            -- TODO: Ensure that variable name collisions in the map also get 
            -- unified rather than overwritten
            istScope . _head <>= M.fromList newParams'
            return ()
        f _ = return ()

applyTyping :: Typing -> Expr -> Maybe Expr
applyTyping (Typing d t) e = e & annLens . domain  %~ unifyDomains (e ^. annLens . domain)
                               & annLens . annType %%~ unifyTypes t

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

