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

-- | Infers data types and privacy domains for expressions in the program.
typeInference :: DatalogProgram -> DatalogProgram
typeInference dp =
  dp & id %~ inferFromInputs
     & dpRules . traversed %~ applyDBInfer
     & id %~ inferFromGoal
     -- & id %~ inferGoal
     -- & dpRules . traversed . ruleTail %~ U.transform inferPred
     -- & dpGoal %~ inferPred
     -- & dpRules . traversed %~ inferRuleRet
  where
    applyDBInfer r = applyTypeSubst (mconcat subst) r
      where
        subst = inferFromDB dp <$> U.universe (r ^. ruleTail)

-- | Infers typings for database predicate arguments from type directives.
-- Takes variable bindings into account
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

-- | Infers typings for database facts
inferPred :: Expr -> Expr
inferPred p@(Pred _ _ xs) = p & annLens . typing .~
  case any boundAndPrivate xs of
    True  -> Typing Private PPBool
    False -> Typing Public PPBool
  where
    boundAndPrivate x = (x ^. annLens . annBound) 
                     && (x ^. annLens . domain . to(==Private))
inferPred x = x

-- | Infers rule types from the parameter typings in the goal predicate.
inferFromGoal :: DatalogProgram -> DatalogProgram
inferFromGoal dp = dp & dpRules . traverse %~ subst
  where 
    g = dp ^. dpGoal
    subst r = fromMaybe r $
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
            s = mconcat $ (uncurry (|->)) <$> paramNames
        return $ applyTypeSubst s r

-- | Creates a new type substitution from expression identifier and typing
(|->) :: String -> Typing -> TypeSubstitution
(|->) x y = TypeSubstitution $ M.singleton x y

-- | Infers types in the program goal by looking at the argument types of
-- rules which get called.
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

inferFromInputs :: DatalogProgram -> DatalogProgram
inferFromInputs dp = dp & dpGoal %~ applyTypeSubstToExpr (mconcat inps)
  where
    inps = dp ^.. inputs . to(\v -> (fromJust $ identifier v) |-> (v ^. annLens . typing))

-- | Decides the return type of a rule by looking at the return types of the
-- predicates in its body
inferRuleRet :: Rule -> Rule
inferRuleRet r = r & ruleHead . annLens . typing .~ unified
  where
    predTypings = [ ann ^. typing | (Pred ann _ _) <- andsToList (r ^. ruleTail) ]
    unified = foldl unifyTypings (Typing Public PPBool) predTypings

-- | Applies a type substitution to an expression
applyTypeSubstToExpr :: TypeSubstitution -> Expr -> Expr
applyTypeSubstToExpr (TypeSubstitution ts) e = U.transform f e
  where
    t :: Expr -> Typing
    t v = fromMaybe (exprTyping v) $ identifier v >>= (`M.lookup` ts)
    f v = applyTyping (t v) v

-- | Gets the resulting typing from unifying two expressions
unifyExprTypings :: Expr -> Expr -> Typing
unifyExprTypings x y = Typing d t
  where
    d = unifyDomains (x ^. annLens . domain) (y ^. annLens . domain)
    t = unifyTypes (x ^. annLens . annType) (y ^. annLens . annType)

-- | Extracts typing from an expression
exprTyping :: Expr -> Typing
exprTyping e = e ^. annLens . typing

-- | Applies type substitution to a rule
applyTypeSubst :: TypeSubstitution -> Rule -> Rule
applyTypeSubst ts r = 
  r & ruleHead %~ applyTypeSubstToExpr ts
    & ruleTail %~ applyTypeSubstToExpr ts

