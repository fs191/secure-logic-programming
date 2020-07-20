module TypeInference 
  ( typeInference
  ) where

import Control.Lens
import Control.Applicative
import Control.Monad

import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe
import qualified Data.Generics.Uniplate.Data as U

import DatalogProgram
import Rule
import Expr
import Annotation
import Language.SecreC.Types

import Debug.Trace

data TypeSubstitution 
  = TypeSubstitution (M.Map String PPType)
  deriving (Show)

instance Semigroup TypeSubstitution where
  (TypeSubstitution x) <> (TypeSubstitution y) = 
    TypeSubstitution $ M.unionWith unifyTypes x y

instance Monoid TypeSubstitution where
  mempty = TypeSubstitution M.empty

typeInference :: DatalogProgram -> DatalogProgram
typeInference dp = inferGoal dp'
  where
    dp' = dp & dpRules . traversed %~ applyDBInfer dp
             & dpRules . traversed %~ applyGoalInfer dp

applyDBInfer :: DatalogProgram -> Rule -> Rule
applyDBInfer dp r = applyTypeSubst (mconcat subst) r
  where
    subst = inferFromDB dp <$> U.universe (r ^. ruleTail)

applyGoalInfer :: DatalogProgram -> Rule -> Rule
applyGoalInfer dp r = applyTypeSubst subst r
  where
    g = dp ^. dpGoal
    subst = inferFromGoal g r

inferFromDB :: DatalogProgram -> Expr -> TypeSubstitution
inferFromDB dp (Pred _ n xs) = mconcat newParams'
  where
    -- Get the database fact corresponding to the predicate
    ext :: [Rule]
    ext = extensionalFacts dp
    matches = L.filter (\x -> ruleName x == n) ext
    _dbargs = matches ^. _head . ruleHead . _Pred . _3 
    -- Unify the argument typings in the database fact and the predicate
    unified = fmap (uncurry unifyExprTypes) (xs `zip` _dbargs)
    -- Take bound and unbound variables into account
    inferParams :: (Expr, PPType) -> TypeSubstitution
    inferParams (v, x) = (fromMaybe (err v) $ identifier v) |-> x
    err v = error $ show v ++ " does not have an indentifier"
    newParams' = inferParams <$> (xs `zip` unified)
    -- Unify the new type substitutions with existing substitutions
inferFromDB _ _ = mempty

inferFromGoal :: Expr -> Rule -> TypeSubstitution
inferFromGoal g r = fromMaybe mempty subst
  where subst =
          do
            rn <- r ^? ruleHead . _Pred . _2
            gn <- g ^? _Pred . _2
            guard $ rn == gn
            rxs <- r ^? ruleHead . _Pred . _3 :: Maybe [Expr]
            gxs <- g ^? _Pred . _3 :: Maybe [Expr]
            let unified = (uncurry unifyExprTypes) <$> (rxs `zip` gxs)
                f (x, y) = (,) <$> (maybeToList $ identifier x) <*> [y]
                paramNames = concat $ traverse f $ rxs `zip` unified
            return . mconcat $ (uncurry (|->)) <$> paramNames

(|->) :: String -> PPType -> TypeSubstitution
(|->) x y = TypeSubstitution $ M.singleton x y

inferGoal :: DatalogProgram -> DatalogProgram
inferGoal dp = dp & dpGoal              %~ f
                  & outputs . traversed %~ f
                  & inputs  . traversed %~ f
  where 
    gxs = dp ^.. dpGoal . _Pred . _3 . folded . to identifier
    n = dp ^. dpGoal . _Pred . _2
    goalRules = zip gxs . L.transpose $ dp ^.. dpRules 
                  . folded 
                  . filtered (\x -> ruleName x == n) 
                  . ruleHead 
                  . _Pred 
                  . _3
    foldUnify x = foldl1 unifyTypes $ x ^.. folded . annLens . annType
    f = applyTypeSubstToExpr $ 
          mconcat [x |-> foldUnify y | (Just x, y) <- goalRules]


applyTypeSubstToExpr :: TypeSubstitution -> Expr -> Expr
applyTypeSubstToExpr sub@(TypeSubstitution ts) e = U.transform f e
  where
    t v = fromMaybe (exprTyping v) $ identifier v >>= (`M.lookup` ts)
    f v = applyTyping (t v) v

applyTyping :: PPType -> Expr -> Expr
applyTyping d e = e & annLens . annType  %~ unifyTypes d

exprTyping :: Expr -> PPType
exprTyping e = e ^. annLens . annType

applyTypeSubst :: TypeSubstitution -> Rule -> Rule
applyTypeSubst ts r = 
  r & ruleHead %~ applyTypeSubstToExpr ts
    & ruleTail %~ applyTypeSubstToExpr ts
