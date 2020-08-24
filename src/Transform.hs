{-# LANGUAGE ScopedTypeVariables #-}

module Transform
  ( deriveAllGroundRules
  ) where

---------------------------------------------------------
---- Transformation of a Datalog script
----   to intermediate representation
---------------------------------------------------------

import Data.Generics.Uniplate.Operations as U
import Data.List
import Data.Maybe

import Control.Lens as L

import Rule
import Expr
import Substitution
import qualified DatalogProgram as DP

-- | Generates all possible ground rules for `n` iterations by inlining
-- predicates with matching rules. Each predicate gets inlined once per
-- iteration.
deriveAllGroundRules :: Int -> DP.DatalogProgram -> DP.DatalogProgram
deriveAllGroundRules n program = program'
                                   -- & fromMaybe program' . adornProgram
                                   & DP.dpRules %~ f
  where
    -- Input program but db clauses are converted to rules
    program' = program
    f :: [Rule] -> [Rule]
    f = foldl (.) id $ replicate n pipeline
    pipeline :: [Rule] -> [Rule]
    pipeline 
      = removeDuplicateFacts 
      . removeFalseFacts 
      . (traversed . ruleTail %~ simplifyAnds :: [Rule] -> [Rule]) 
      . map (refreshRule "X_") 
      -- . (traversed %~ simplifyVars)
      . inlineOnce

-- | Tries to unify each predicate in each rule body with an appropriate rule
inlineOnce :: [Rule] -> [Rule]
inlineOnce rs =
  rs <> inlined
  where
    inlined = catMaybes $ do
      tgt <- refreshRule "T_" <$> rs
      src <- rs
      let shd = src ^. ruleHead
      let stl = src ^. ruleTail
      let ttl = tgt ^. ruleTail
      (p@Pred{}, mut) <- U.contexts ttl
      let subst = unify shd p
      [flip applySubst (tgt & ruleTail .~ mut stl) <$> subst]

--simplifyVars :: Rule -> Rule
--simplifyVars r = applySubst subst r
--  where subst = simplifyVars' $ r ^. ruleTail
--
---- | Removes any unnecessary variable equalities (e.g. X=Y)
--simplifyVars' :: Expr -> Subst
--simplifyVars' r = compress . mconcat $ f <$> U.universe r
--  where f (Eq _ v x)
--          | isVar x  = v |-> x
--          | otherwise = mempty
--        f (Eq _ x v)
--          | isVar x  = v |-> x
--          | otherwise = mempty
--        f _ = mempty

-- Removes duplicate terms from AND operations at the root expression
simplifyAnds :: Expr -> Expr
simplifyAnds x = foldr1 eAnd . nub . filter (not . isAnd) $ simplifyAnds' x

simplifyAnds' :: Expr -> [Expr]
simplifyAnds' (And _ x y) = simplifyAnds' x <> simplifyAnds' y
simplifyAnds' x = [x]

isAnd :: Expr -> Bool
isAnd And{} = True
isAnd _     = False

-- | Removes facts that always evaluate to False
removeFalseFacts :: [Rule] -> [Rule]
removeFalseFacts = filter (\x -> x ^. ruleTail /= constBool False)

-- | Removes duplicates of facts that appear more than once
-- TODO make it preserve the order of predicates
removeDuplicateFacts :: [Rule] -> [Rule]
removeDuplicateFacts = nub

-- | Binds constants that are arguments of some predicate to a new variable
--bindArgColumns :: Expr -> State Int Expr
--bindArgColumns (Pred ann n as) =
--  do
--    let freshVar :: State Int Expr
--        freshVar =
--          do
--            i <- get
--            put $ i + 1
--            return . var $ "_CONST_" <> show i
--    let cols = [x | x@(ConstStr _ _) <- as]
--    vars <- sequenceA $ replicate (length cols) freshVar
--    let cvs   = cols `zip` vars
--        lkp :: Expr -> Expr
--        lkp x = fromMaybe x $ lookup x cvs
--        newAs = lkp <$> as
--        eqs   = uncurry equal <$> cvs
--        newPred = Pred ann n newAs
--    return $ foldr eAnd newPred eqs
--bindArgColumns x = return x

