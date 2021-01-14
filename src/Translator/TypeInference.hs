{-# LANGUAGE FlexibleContexts #-}
-- | Type and privacy domain inference for privalog programs. 
module Translator.TypeInference 
  ( typeInference
  , clearTypings
  ) where

import Relude

import Control.Lens

import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe
import qualified Data.Generics.Uniplate.Data as U

import Annotation
import Expr
import DatalogProgram
import Rule
import Language.Privalog.Types
import ErrorMsg

import Data.Text.Prettyprint.Doc

-- | A substitution for typings. Substitutes types based on expression 
-- identifier. It does not overwrite the typing, but rather tries to unify the 
-- new typing with the old.
newtype TypeSubstitution 
  = TypeSubstitution (M.Map Text Typing)
  deriving (Show)

instance Semigroup TypeSubstitution where
  (TypeSubstitution x) <> (TypeSubstitution y) = 
    TypeSubstitution $ M.unionWith (\a b -> fromMaybe err $ unifyTypings a b) x y
    where
      err = error $ "Failed to unify substitutions " <> show x <> " and " <> show y

instance Monoid TypeSubstitution where
  mempty = TypeSubstitution M.empty

-- | Infers data types and privacy domains for most expressions in the program.
-- Assumes that the program has been post-processed.
typeInference 
  :: DatalogProgram 
  -> DatalogProgram
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
    inferParams (v, Typing d t) = 
      fromMaybe (err v) (identifier v) |-> Typing y t
      where 
        y | v ^. annotation . annBound = Unknown
          | otherwise     = d
    err v = error $ 
      show (v ^. annotation . srcPos) <> " "
      <> show v <> " does not have an indentifier"
    newParams' = inferParams <$> (xs `zip` unified)
inferFromDB _ _ = mempty

-- | Sets typings of all constants to their default value.
inferConstants :: Expr -> Expr
inferConstants x@(ConstStr _ _) = err $ applyTyping (Typing Public PPStr) x
  where
    err = fromMaybe $ error "Failed to apply typing"
inferConstants x@(ConstInt _ _) 
  | isNumericType (x ^. annotation . annType) = err $ applyTyping (Typing Public PPAuto) x
  | otherwise                                 = err $ applyTyping (Typing Public PPInt64) x
  where
    err = fromMaybe $ error "Failed to apply typing"
inferConstants x@(ConstFloat _ _)
  | isNumericType (x ^. annotation . annType) = err $ applyTyping (Typing Public PPAuto) x
  | otherwise                                 = err $ applyTyping (Typing Public PPFloat64) x
  where
    err = fromMaybe $ error "Failed to apply typing"
inferConstants (ConstBool _ x) = constBool x
-- Ensure that the return type of all predicates is boolean
inferConstants (Pred _ n xs) = predicate n xs
-- Square root should return a float
inferConstants (Sqrt _ x) = eSqrt x
inferConstants x = x

-- | Infers domains for database clauses
inferDBRet :: DatalogProgram -> DatalogProgram
inferDBRet dp = dp & dpRules . traversed . ruleTail %~ U.transform f
  where
    f p@(Pred _ n xs) 
      | isIDBFact dp p = p
      | anyPC          = p & annotation . domain .~ Private
      | anyUnk         = p
      | otherwise      = p & annotation . domain .~ Public
      where
        dbf = fromMaybe err $ findDBFact dp n
        err = error $ "DB fact not found: " <> show n <> "\n" <> show (pretty dp)
        (Pred _ _ ys) = dbf ^. ruleHead
        -- See if a bound variable is compared to a private DB column
        privateComp (x, y) = 
          x ^. annotation . annBound &&
                               ( x ^. annotation . domain . to (==Private) ||
                                 y ^. annotation . domain . to (==Private))


        anyPC = any privateComp $ xs `zip` ys
        anyUnk = elem Unknown $ xs ^.. folded . annotation . domain
    f x = x

applyUn :: (Expr -> Expr -> b) -> Expr -> Maybe b
applyUn f e =
  do
    x <- e ^? arg
    return $ f e x

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
-- Warning: if the type checker gets stuck in a loop, then it is probably
-- due to this function. It gets called until it reaches a fixpoint, so make
-- sure that it converges.
inferBuiltins :: Rule -> Rule
inferBuiltins r = r & ruleTail %~ U.transform ret
                    & id       %~ U.transform subst'
                    & ruleTail %~ U.transform rewr
  where
    ret :: Expr -> Expr
    ret x = fromMaybe x $ applyBin inferBinRet x
    subst :: [Expr] -> TypeSubstitution
    subst x = bin <> unry
      where
        bin = mconcat . catMaybes $ applyBin inferBinArgs <$> x
        unry  = mconcat . catMaybes $ applyUn inferUnArg <$> x
    subst' :: Rule -> Rule
    subst' x = applyTypeSubst (x ^. ruleTail . to (subst . U.universe)) x
    rewr e@(Sqrt a x) = Sqrt (a & typing %~ fromMaybe err . unifyTypings xt) x
      where err = error . show $ TypeApplicationFailed (xt ^. tType) e
            xt = x ^. annotation . typing
    rewr e@(Reshare a x) = fromMaybe e $ asum
      [ reshareBetween PPXorUInt64 PPUInt64
      , reshareBetween PPXorUInt32 PPUInt32
      , reshareBetween PPXorUInt16 PPUInt16
      , reshareBetween PPXorUInt8  PPUInt8
      ]
      where
        reshareBetween t1 t2
          | x ^. annotation . annType . to (==t1) = Just $ Reshare ( a & annType .~ t2
                                                                       & domain  .~ Private
                                                                   ) x
          | x ^. annotation . annType . to (==t2) = Just $ Reshare ( a & annType .~ t1
                                                                       & domain  .~ Private
                                                                   ) x
          | otherwise = Nothing
    rewr x = x

inferBinRet :: Expr -> Expr -> Expr -> Expr
-- TODO this is a workaround for 'choose' operator
-- TODO we also need to infer the boolean domain
inferBinRet e@Choose{} (Expr.List _ xs) (Expr.List _ _) =
    e & annotation . annType %~ fromMaybe err2 . unifyTypes t
      & annotation . domain  .~ d
    where
      ts = map (\x -> x ^. annotation . annType) xs
      ds = map (\x -> x ^. annotation . domain)  xs

      t = L.foldr1 (\t1 t2 -> fromMaybe (err t1 t2) $ unifyTypes t1 t2) ts
      d = L.foldr1 safelyUnifyDomains ds

      err t1 t2  = error $ "Failed to unify expressions " <> show t1 <> " and " <> show t2
      err2       = error $ "Failed to apply typing " <> show t <> " to " <> show e
inferBinRet e x y
  -- If both are bound then it is a comparison and privacy depends on subterms
  | xb && yb  = e & annotation . annType %~ fromMaybe err2 . unifyTypes ut
                  & annotation . domain  .~ safelyUnifyDomains xd yd
  -- If only one variable is bound, then it is an assignment and will always
  -- return true
  -- except when the assignee is a Choose construction
  | xb        = e & annotation . annType %~ fromMaybe err2 . unifyTypes ut
                  & annotation . domain  .~ d
  | yb        = e & annotation . annType %~ fromMaybe err2 . unifyTypes ut
                  & annotation . domain  .~ d
  -- User has written incorrect code if both sides of the expression are unbound
  | otherwise = error $ "Uninitialized subexpressions: " <> show e
  where
    xd = x ^. annotation . domain
    xb = x ^. annotation . annBound
    xt = x ^. annotation . annType
    yd = y ^. annotation . domain
    yb = y ^. annotation . annBound
    yt = y ^. annotation . annType

  -- TODO we need to handle Choose in a nicer way
    d | has _Choose x =
             case x of
                 Choose _ _ (Expr.List _ bs) -> 
                     let ds = map (\b -> b ^. annotation . domain) bs in
                     L.foldr1 safelyUnifyDomains ds
                 _ -> Public
      | has _Choose y =
             case y of
                 Choose _ _ (Expr.List _ bs) -> 
                     let ds = map (\b -> b ^. annotation . domain) bs in
                     L.foldr1 safelyUnifyDomains ds
                 _ -> Public
      | otherwise = Public

    ut | isArithmetic e  = fromMaybe err $ unifyTypes xt yt
       | isPredicative e = PPBool
       | otherwise       = PPAuto
    err = error $ "Failed to unify expressions " <> show x <> " and " <> show y
    err2 = error $ "Failed to apply typing " <> show ut <> " to " <> show e

inferBinArgs :: Expr -> Expr -> Expr -> TypeSubstitution
inferBinArgs e x y
  | xb && yb = mempty
  | yb       = maybe mempty (|-> yt) (identifier x)
  | xb       = maybe mempty (|-> xt) (identifier y)
  | otherwise = error $ "Uninitialized subexpressions: " <> show e
  where
    xt = x ^. annotation . typing
    xb = x ^. annotation . annBound
    yt = y ^. annotation . typing
    yb = y ^. annotation . annBound

inferUnArg :: Expr -> Expr -> TypeSubstitution
inferUnArg Reshare{} x = maybe mempty (|-> xt) (identifier x)
  where 
    xt = x ^. annotation . typing . to (tDom .~ Private)
inferUnArg _ _           = mempty

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
        let unified = uncurry unifyExprTypings <$> (rxs `zip` gxs)
            f (x, y) = (,) <$> maybeToList (identifier x) <*> [y]
            paramNames :: [(Text, Typing)]
            paramNames = concat $ traverse f $ rxs `zip` unified
            s = mconcat $ uncurry (|->) <$> paramNames
        return $ applyTypeSubst s r

-- | Creates a new type substitution from an expression identifier and a typing
(|->) :: Text -> Typing -> TypeSubstitution
(|->) x y = TypeSubstitution $ M.singleton x y

-- | Infers types in the program goal by looking at the argument types of
-- rules that get called.
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
    foldUnify (x:xt) = foldl' (unifyWithError unifyTypings) (x ^. annotation . typing) xt
    foldUnify [] = error "No expressions to unify"
    f = applyTypeSubstToExpr .
          mconcat $ [x |-> foldUnify y | (Just x, y) <- goalRules]

unifyWithError :: (Typing -> Typing -> Maybe Typing) -> Typing -> Expr -> Typing
unifyWithError f x y = fromMaybe err $ f x (y ^. annotation . typing)
  where
    err = error $ "Could not apply typing " <> show x <> " to " <> show y

-- | Infer types from the input directive
inferFromInputs :: DatalogProgram -> DatalogProgram
inferFromInputs dp = dp & dpGoal %~ applyTypeSubstToExpr (mconcat inps)
  where
    inps = dp ^.. inputs . to(\v -> fromJust (identifier v) |-> (v ^. annotation . typing))

-- | Decides the return type of a rule by looking at the return types of the
-- predicates in its body
inferRuleRet :: Rule -> Rule
inferRuleRet r = r & ruleHead . annotation . typing .~ unified'
  where
    (x:xt) = andsToList (r ^. ruleTail)
    x' = r ^. ruleHead . annotation . typing
    unified = foldl' (unifyWithError safelyUnifyTypings) (x ^. annotation . typing) xt
    unified' = fromMaybe err $ unifyTypings x' unified
    err = error $ "Failed to unify head typing " 
               <> show x' <> " with body typing " <> show unified'

-- | Infers the return typing of the goal statement
inferGoalRet :: DatalogProgram -> DatalogProgram
inferGoalRet dp = dp & dpGoal . annotation . domain .~ d
  where
    doms = dp ^.. dpRules . folded . ruleHead . annotation . domain
    d = foldl' safelyUnifyDomains Public doms

-- | Applies a type substitution to an expression
applyTypeSubstToExpr :: TypeSubstitution -> Expr -> Expr
applyTypeSubstToExpr (TypeSubstitution ts) = U.transform f
  where
    t :: Expr -> Typing
    t v = fromMaybe (exprTyping v) $ identifier v >>= (`M.lookup` ts)
    f v = v & annotation . typing .~ unifyWithError unifyTypings (t v) v

-- | Gets the resulting typing from unifying two expressions
unifyExprTypings :: Expr -> Expr -> Typing
unifyExprTypings x y = Typing d t
  where
    d = unifyDomains (x ^. annotation . domain) (y ^. annotation . domain)
    t = fromMaybe err $ unifyTypes (x ^. annotation . annType) (y ^. annotation . annType)
    err = error $ "Failed to unify expressions " <> show x <> " and " <> show y

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

clearTypings :: DatalogProgram -> DatalogProgram
clearTypings dp = dp & dpRules %~ U.transformBi clearTypings'
                     & dpGoal  %~ U.transform clearTypings'
                     & outputs  %~ U.transform clearTypings'

clearTypings' :: Expr -> Expr
clearTypings' = U.transform f
  where
    f = annotation . typing .~ emptyTyping

