-- | Type and privacy domain inference for privalog programs. 
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

-- | A substitution for typings. Substitutes types based on expression 
-- identifier. It does not overwrite the typing, but rather tries to unify the 
-- new typing with the old.
data TypeSubstitution 
  = TypeSubstitution (M.Map String Typing)
  deriving (Show)

instance Semigroup TypeSubstitution where
  (TypeSubstitution x) <> (TypeSubstitution y) = 
    TypeSubstitution $ M.unionWith unifyTypings x y

instance Monoid TypeSubstitution where
  mempty = TypeSubstitution M.empty

-- | Infers data types and privacy domains for most expressions in the program.
-- Assumes that the program has been post-processed.
typeInference :: DatalogProgram -> DatalogProgram
typeInference dp = dp
     -- Infer typings for all constants
     & dpRules . traversed . ruleHead %~ U.transform inferConstants
     & dpRules . traversed . ruleTail %~ U.transform inferConstants
     & dpGoal %~ U.transform inferConstants
     -- Take typings from the input clause and unify them with attributes
     -- in the goal.
     & id %~ inferFromInputs
     -- Get typings from database clauses
     & dpRules . traversed %~ applyDBInfer
     -- Apply the typings in the goal clause to all of the rule clauses
     & id %~ inferFromGoal
     -- Infer typings of binary sub-expressions (addition, multiplication etc.)
     -- Warning: if the type checker gets stuck in a loop, then it is probably
     -- due to this function.
     -- TODO: Reimplement using term rewriting
     & dpRules . traversed %~ converge inferBuiltins
     -- Infer the return typings of database queries in the rule bodies.
     & id %~ inferDBRet
     -- Infer the return typings of rule heads
     & dpRules . traversed %~ inferRuleRet
     -- Look at the rules that get called by the goal to deduce the typings
     -- of the parameters in the goal.
     & id %~ inferGoal
     -- Derive the return type of the goal by looking at the return types
     -- of the rules that get called.
     & id %~ inferGoalRet
  where
    applyDBInfer r = applyTypeSubst (mconcat subst) r
      where
        subst = inferFromDB dp <$> U.universe (r ^. ruleTail)

-- | Infers typings for database predicate arguments from type directives.
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
    -- Take bound and unbound variables into account.
    -- If the parameter is unbound, then it takes its domain from the schema,
    -- otherwise nothing changes
    inferParams :: (Expr, Typing) -> TypeSubstitution
    inferParams (v, (Typing d t)) = 
      (fromMaybe (err v) $ identifier v) |-> Typing y t
      where 
        y | v ^. annotation . annBound = Unknown
          | otherwise     = d
    err v = error $ show v ++ " does not have an indentifier"
    newParams' = inferParams <$> (xs `zip` unified)
inferFromDB _ _ = mempty

-- | Sets typings of all constants to their default value (always public).
inferConstants :: Expr -> Expr
inferConstants (ConstStr _ x) = constStr x
inferConstants (ConstInt _ x) = constInt x
inferConstants (ConstBool _ x) = constBool x
-- Ensure that the return type of all predicates is boolean
inferConstants (Pred _ n xs) = predicate n xs 
inferConstants x = x

-- | Infers domains for database clauses
inferDBRet :: DatalogProgram -> DatalogProgram
inferDBRet dp = dp & dpRules . traversed . ruleTail %~ U.transform f
  where
    f p@(Pred _ n xs) 
      | anyPC     = p & annotation . domain .~ Private
      | anyUnk    = p
      | otherwise = p & annotation . domain .~ Public
      where
        dbf = findDBFact dp n
        (Pred _ _ ys) = dbf ^. ruleHead
        -- See if a bound variable is compared to a private DB column
        privateComp (x, y) = x ^. annotation . annBound && 
                             y ^. annotation . domain . to (==Private)
        anyPC = any privateComp $ xs `zip` ys
        anyUnk = any (==Unknown) $ xs ^.. folded . annotation . domain
    f x = x

applyBin :: (Expr -> Expr -> Expr -> b) -> Expr -> Maybe b
applyBin f e =
  do
    l <- e ^? leftHand
    r <- e ^? rightHand
    return $ f e l r

-----------------------------------------------
-- Built-in predicate type and domain inference
-----------------------------------------------

-- | Infers the return types for binary subexpressions
inferBuiltins :: Rule -> Rule
inferBuiltins r = r & ruleTail %~ U.transform ret
                    & id %~ U.transform subst'
  where
    ret :: Expr -> Expr
    ret x = fromMaybe x $ applyBin inferBinRet x
    subst :: [Expr] -> TypeSubstitution
    subst x = mconcat . catMaybes $ applyBin inferBinArgs <$> x
    subst' :: Rule -> Rule
    subst' x = applyTypeSubst (x ^. ruleTail . to (subst . U.universe)) x

inferBinRet :: Expr -> Expr -> Expr -> Expr
inferBinRet e x y
  -- If both are bound then it is a comparison and privacy depends on subterms
  | xb && yb  = e & annotation . annType %~ unifyTypes ut
                  & annotation . domain  .~ safelyUnifyDomains xd yd
  -- If only one variable is bound, then it is an assignment and will always
  -- return true
  | xb        = e & annotation . annType %~ unifyTypes ut
                  & annotation . domain  .~ Public
  | yb        = e & annotation . annType %~ unifyTypes ut
                  & annotation . domain  .~ Public
  -- User has written incorrect code if both sides of the expression are unbound
  | otherwise = error $ "Uninitialized subexpressions: " ++ show e
  where
    xd = x ^. annotation . domain
    xb = x ^. annotation . annBound
    xt = x ^. annotation . annType
    yd = y ^. annotation . domain
    yb = y ^. annotation . annBound
    yt = y ^. annotation . annType
    ut | isArithmetic e  = unifyTypes xt yt
       | isPredicative e = PPBool
       | otherwise       = PPAuto

inferBinArgs :: Expr -> Expr -> Expr -> TypeSubstitution
inferBinArgs e x y
  | xb && yb = mempty
  | yb       = fromMaybe mempty $ (|-> yt) <$> identifier x
  | xb       = fromMaybe mempty $ (|-> xt) <$> identifier y
  | otherwise = error $ "Uninitialized subexpressions: " ++ show e
  where
    xt = x ^. annotation . typing
    xb = x ^. annotation . annBound
    yt = y ^. annotation . typing
    yb = y ^. annotation . annBound

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

-- | Creates a new type substitution from an expression identifier and a typing
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
    foldUnify x = foldl1 unifyTypings $ x ^.. folded . annotation . typing
    f = applyTypeSubstToExpr .
          mconcat $ [x |-> (foldUnify y) | (Just x, y) <- goalRules]

-- | Infer types from the input directive
inferFromInputs :: DatalogProgram -> DatalogProgram
inferFromInputs dp = dp & dpGoal %~ applyTypeSubstToExpr (mconcat inps)
  where
    inps = dp ^.. inputs . to(\v -> (fromJust $ identifier v) |-> (v ^. annotation . typing))

-- | Decides the return type of a rule by looking at the return types of the
-- predicates in its body
inferRuleRet :: Rule -> Rule
inferRuleRet r = r & ruleHead . annotation . typing %~ unified
  where
    predTypings =
      do 
        x <- andsToList (r ^. ruleTail)
        return $ x ^. annotation . typing
    unified = unifyTypings $ foldl1 safelyUnifyTypings predTypings

-- | Infers the return typing of the goal statement
inferGoalRet :: DatalogProgram -> DatalogProgram
inferGoalRet dp = dp & dpGoal . annotation . domain .~ d
  where
    doms = dp ^.. dpRules . folded . ruleHead . annotation . domain
    d = foldl safelyUnifyDomains Public doms

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
    d = unifyDomains (x ^. annotation . domain) (y ^. annotation . domain)
    t = unifyTypes (x ^. annotation . annType) (y ^. annotation . annType)

-- | Extracts typing from an expression
exprTyping :: Expr -> Typing
exprTyping e = e ^. annotation . typing

-- | Applies type substitution to a rule
applyTypeSubst :: TypeSubstitution -> Rule -> Rule
applyTypeSubst ts r = 
  r & ruleHead %~ applyTypeSubstToExpr ts
    & ruleTail %~ applyTypeSubstToExpr ts

-- | Applies a function until a fixpoint is reached
converge :: (Eq a) => (a -> a) -> a -> a
converge f a
  | f a == a  = a
  | otherwise = converge f $ f a

