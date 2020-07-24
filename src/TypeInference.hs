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
  dp & dpRules . traversed . ruleHead %~ U.transform inferConstants
     & dpRules . traversed . ruleTail %~ U.transform inferConstants
     & dpGoal %~ U.transform inferConstants
     & id %~ inferFromInputs
     & dpRules . traversed %~ applyDBInfer
     & id %~ inferFromGoal
     & dpRules . traversed %~ inferBuiltins
     & id %~ inferPred
     & dpRules . traversed %~ inferRuleRet
     & id %~ inferGoal
     & id %~ inferGoalRet
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
        y | v ^. ann . annBound = Unknown
          | otherwise     = d
    err v = error $ show v ++ " does not have an indentifier"
    newParams' = inferParams <$> (xs `zip` unified)
    -- Unify the new type substitutions with existing substitutions
inferFromDB _ _ = mempty

inferConstants :: Expr -> Expr
inferConstants (ConstStr _ x) = constStr x
inferConstants (ConstInt _ x) = constInt x
inferConstants (ConstBool _ x) = constBool x
inferConstants (Pred _ n xs) = predicate n xs
inferConstants x = x

-- | Infers typings for database facts
inferPred :: DatalogProgram -> DatalogProgram
inferPred dp = dp & dpRules . traversed . ruleTail %~ U.transform f
  where
    f p@(Pred _ n xs) 
      | anyPC     = p & ann . domain .~ Private
      | anyUnk    = p
      | otherwise = p & ann . domain .~ Public
      where
        dbf = findDBFact dp n
        (Pred _ _ ys) = dbf ^. ruleHead
        -- See if a bound variable is compared to a private DB column
        privateComp (x, y) = x ^. ann . annBound && 
                             y ^. ann . domain . to (==Private)
        anyPC = any privateComp $ xs `zip` ys
        anyUnk = any (==Unknown) $ xs ^.. folded . ann . domain
    f x = x

-----------------------------------------------
-- Built-in predicate type and domain inference
-----------------------------------------------

appIfBinPred :: (Expr -> Expr -> Expr -> a) -> Expr -> Maybe a
appIfBinPred f e@(Ge _ a b) = Just $ f e a b
appIfBinPred f e@(Gt _ a b) = Just $ f e a b
appIfBinPred f e@(Eq _ a b) = Just $ f e a b
appIfBinPred f e@(Le _ a b) = Just $ f e a b
appIfBinPred f e@(Lt _ a b) = Just $ f e a b
appIfBinPred _ _ = Nothing

inferBuiltins :: Rule -> Rule
inferBuiltins r = applyTypeSubst subst' r'
  where
    ret x = fromMaybe x $ appIfBinPred inferBinRet x
    r' = r & ruleTail %~ U.transform ret
    subst x = fromMaybe mempty $ appIfBinPred inferBinArgs x
    subst' = mconcat $ subst <$> r ^. ruleTail . to U.universe

inferBinRet :: Expr -> Expr -> Expr -> Expr
inferBinRet e x y
  -- If both are bound then it is a comparison and privacy depends on subterms
  | xb && yb  = e & ann . annType %~ unifyTypes PPBool
                  & ann . domain  .~ safelyUnifyDomains xd yd
  -- If only one variable is bound, then it is an assignment and will always
  -- return true
  | xb        = e & ann . annType %~ unifyTypes PPBool
                  & ann . domain  .~ Public
  | yb        = e & ann . annType %~ unifyTypes PPBool
                  & ann . domain  .~ Public
  | otherwise = error $ "Uninitialized variables: " ++ show e
  where
    xd = x ^. ann . domain
    xb = x ^. ann . annBound
    yd = y ^. ann . domain
    yb = y ^. ann . annBound

inferBinArgs :: Expr -> Expr -> Expr -> TypeSubstitution
inferBinArgs e x y
  | xb && yb = mempty
  | yb       = fromMaybe mempty $ (|-> yt) <$> identifier x
  | xb       = fromMaybe mempty $ (|-> xt) <$> identifier y
  | otherwise = error $ "Uninitialized variables: " ++ show e
  where
    xt = x ^. ann . typing
    xb = x ^. ann . annBound
    yt = y ^. ann . typing
    yb = y ^. ann . annBound

-- | Infers rule types from the parameter typings in the goal predicate.
inferFromGoal :: DatalogProgram -> DatalogProgram
inferFromGoal dp = dp & dpRules . traverse %~ subst
  where 
    g = dp ^. dpGoal
    subst r = fromMaybe r $
      do
        -- Find all rules that get called by the goal
        rn <- r ^? ruleHead . predName
        gn <- g ^? predName
        guard $ rn == gn
        rxs <- r ^? ruleHead . predArgs
        gxs <- g ^? predArgs
        -- Unify goal and rule arguments
        let unified = (uncurry unifyExprTypings) <$> (rxs `zip` gxs)
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
    foldUnify :: [Expr] -> PPType
    foldUnify x = foldl1 unifyTypes $ x ^.. folded . ann . annType
    f = applyTypeSubstToExpr .
          mconcat $ [x |-> (Typing Unknown $ foldUnify y) | (Just x, y) <- goalRules]

inferFromInputs :: DatalogProgram -> DatalogProgram
inferFromInputs dp = dp & dpGoal %~ applyTypeSubstToExpr (mconcat inps)
  where
    inps = dp ^.. inputs . to(\v -> (fromJust $ identifier v) |-> (v ^. ann . typing))

-- | Decides the return type of a rule by looking at the return types of the
-- predicates in its body
inferRuleRet :: Rule -> Rule
inferRuleRet r = r & ruleHead . ann . typing %~ unified
  where
    isPred x = x ^. ann . annType . to(==PPBool)
    predTypings = catMaybes [ 
      do 
        guard (isPred x)
        Just $ x ^. ann . typing 
      | x <- andsToList (r ^. ruleTail) ]
    unified
      | any (\(Typing d _) -> d == Unknown) predTypings = id
      | otherwise = const $ foldl unifyTypings (Typing Unknown PPBool) predTypings

inferGoalRet :: DatalogProgram -> DatalogProgram
inferGoalRet dp = dp & dpGoal . ann . domain .~ d
  where
    doms = dp ^.. dpRules . folded . ruleHead . ann . domain
    d = foldl safelyUnifyDomains Unknown doms

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
    d = unifyDomains (x ^. ann . domain) (y ^. ann . domain)
    t = unifyTypes (x ^. ann . annType) (y ^. ann . annType)

-- | Extracts typing from an expression
exprTyping :: Expr -> Typing
exprTyping e = e ^. ann . typing

-- | Applies type substitution to a rule
applyTypeSubst :: TypeSubstitution -> Rule -> Rule
applyTypeSubst ts r = 
  r & ruleHead %~ applyTypeSubstToExpr ts
    & ruleTail %~ applyTypeSubstToExpr ts

safelyUnifyDomains :: PPDomain -> PPDomain -> PPDomain
safelyUnifyDomains Private _ = Private
safelyUnifyDomains _ Private = Private
safelyUnifyDomains Unknown _ = Unknown
safelyUnifyDomains _ Unknown = Unknown
safelyUnifyDomains _ _       = Public

