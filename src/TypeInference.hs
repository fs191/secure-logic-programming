module TypeInference 
  ( typeInference
  ) where

import Control.Lens
import Control.Monad

import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe
import qualified Data.Generics.Uniplate.Data as U

import Annotation
import Expr
import DatalogProgram
import Rule
import Language.SecreC.Types

data TypeSubstitution 
  = TypeSubstitution (M.Map String Typing)
  deriving (Show)

instance Semigroup TypeSubstitution where
  (TypeSubstitution x) <> (TypeSubstitution y) = 
    TypeSubstitution $ M.unionWith unifyTypings x y

instance Monoid TypeSubstitution where
  mempty = TypeSubstitution M.empty

typeInference :: DatalogProgram -> DatalogProgram
typeInference dp = inferGoal dp'
  where
    dp' = dp & dpRules . traversed %~ applyDBInfer dp
             & dpRules . traversed %~ applyGoalInfer dp
             & dpRules . traversed . ruleTail %~ predInf
             & dpGoal %~ predInf
    predInf e = applyTypeSubstToExpr (inferPred e) e

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
    unified :: [Typing]
    unified = fmap (uncurry unifyExprTypings) (xs `zip` _dbargs)
    -- Take bound and unbound variables into account
    inferParams :: (Expr, Typing) -> TypeSubstitution
    inferParams (v, (Typing d t)) = 
      (fromMaybe (err v) $ identifier v) |-> Typing y t
      where 
        y | v ^. annLens . annBound = Unknown
          | otherwise     = d
    err v = error $ show v ++ " does not have an indentifier"
    newParams' = inferParams <$> (xs `zip` unified)
    -- Unify the new type substitutions with existing substitutions
inferFromDB _ _ = mempty

inferPred :: Expr -> TypeSubstitution
inferPred (Pred _ n xs) = 
  case any boundAndPrivate xs of
    True  -> n |-> Typing Private PPAuto
    False -> n |-> Typing Public PPAuto
  where
    boundAndPrivate x = (x ^. annLens . annBound) 
                     && (x ^. annLens . domain . to(==Private))
inferPred _ = mempty

inferFromGoal :: Expr -> Rule -> TypeSubstitution
inferFromGoal g r = fromMaybe mempty subst
  where subst =
          do
            rn <- r ^? ruleHead . _Pred . _2
            gn <- g ^? _Pred . _2
            guard $ rn == gn
            rxs <- r ^? ruleHead . _Pred . _3 :: Maybe [Expr]
            gxs <- g ^? _Pred . _3 :: Maybe [Expr]
            let unified :: [Typing]
                unified = (uncurry unifyExprTypings) <$> (rxs `zip` gxs)
                f (x, y) = (,) <$> (maybeToList $ identifier x) <*> [y]
                paramNames :: [(String, Typing)]
                paramNames = concat $ traverse f $ rxs `zip` unified
            return . mconcat $ (uncurry (|->)) <$> paramNames

(|->) :: String -> Typing -> TypeSubstitution
(|->) x y = TypeSubstitution $ M.singleton x y

inferGoal :: DatalogProgram -> DatalogProgram
inferGoal dp = dp & dpGoal  %~ U.transform f
                  & outputs %~ f
  where 
    gxs = dp ^.. dpGoal . _Pred . _3 . folded . to identifier
    n = dp ^. dpGoal . _Pred . _2
    goalRules = zip gxs . L.transpose $ dp ^.. dpRules 
                  . folded 
                  . filtered (\x -> ruleName x == n) 
                  . ruleHead 
                  . _Pred 
                  . _3
    foldUnify :: [Expr] -> Typing
    foldUnify x = foldl1 unifyTypings $ x ^.. folded . annLens . typing
    f = applyTypeSubstToExpr .
          mconcat $ [x |-> foldUnify y | (Just x, y) <- goalRules]


applyTypeSubstToExpr :: TypeSubstitution -> Expr -> Expr
applyTypeSubstToExpr (TypeSubstitution ts) e = U.transform f e
  where
    t :: Expr -> Typing
    t v = fromMaybe (exprTyping v) $ identifier v >>= (`M.lookup` ts)
    f v = applyTyping (t v) v

unifyExprTypings :: Expr -> Expr -> Typing
unifyExprTypings x y = Typing d t
  where
    d = unifyDomains (x ^. annLens . domain) (y ^. annLens . domain)
    t = unifyTypes (x ^. annLens . annType) (y ^. annLens . annType)

unifyTypings :: Typing -> Typing -> Typing
unifyTypings (Typing xd xt) (Typing yd yt)
  =  Typing (unifyDomains xd yd) (unifyTypes xt yt)

applyTyping :: Typing -> Expr -> Expr
applyTyping (Typing d t) e = 
  e & annLens . annType .~ t
    & annLens . domain  .~ d

exprTyping :: Expr -> Typing
exprTyping e = Typing ed et
  where
    ed = e ^. annLens . domain
    et = e ^. annLens . annType
              

applyTypeSubst :: TypeSubstitution -> Rule -> Rule
applyTypeSubst ts r = 
  r & ruleHead %~ applyTypeSubstToExpr ts
    & ruleTail %~ applyTypeSubstToExpr ts
