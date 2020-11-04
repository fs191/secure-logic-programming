{-# LANGUAGE ScopedTypeVariables #-}

module Transform
  ( deriveAllGroundRules
  , InliningStrategy(..)
  ) where

---------------------------------------------------------
---- Transformation of a Datalog script
----   to intermediate representation
---------------------------------------------------------

import Relude

import Data.Generics.Uniplate.Operations as U

import qualified Data.List as L

import Control.Lens

import Rule
import Expr
import Solve
import Substitution
import DatalogProgram
import Annotation
import Simplify

data InliningStrategy 
  = BreadthFirst
  | DepthFirst

-- | Generates all possible ground rules for `n` iterations by inlining
-- predicates with matching rules. Each predicate gets inlined once per
-- iteration.
deriveAllGroundRules 
  :: (MonadFail m, MonadIO m) 
  => Int 
  -> DatalogProgram 
  -> m DatalogProgram
deriveAllGroundRules n program = f program'
  where
    -- Input program but db clauses are converted to rules
    program' = program
    f rs = loopPipeline n rs

pipeline 
  :: (MonadFail m, MonadIO m)
  => DatalogProgram 
  -> m DatalogProgram
pipeline dp = 
  do
    pl0 <- dp & id %~ inlineOnceBFS
             & dpRules . traversed %~ refreshRule "X_"
             & dpRules %%~ filterM checkConsistency
    pl  <- pl0 & dpRules . traversed %%~ simplifySat
    return $ pl & dpRules . traversed . ruleTail %~ simplifyAnds
                & dpRules %~ simplifyRules
                & dpRules %~ removeFalseFacts
                & dpRules %~ L.nub

loopPipeline :: (MonadFail m, MonadIO m) => Int  -> DatalogProgram -> m DatalogProgram
loopPipeline n dp = do
    dp' <- pipeline dp
    let groundRulesBefore = filter (isGround dp ) (dp  ^. dpRules)

    -- TODO maybe, we can find a better stop condition and do not need to compute hashes on each step
    -- for the Gr strategy, we could use e.g (length groundRulesBefore == length groundRulesAfter)
    if (length groundRulesBefore >= n) || (dp == dp) then do
        return dp

    else do
        loopPipeline n dp'

-- | Tries to unify the first predicate in each rule body with an appropriate rule using Breadth-First-Search strategy
inlineOnceBFS :: DatalogProgram -> DatalogProgram
inlineOnceBFS dp = dp & dpRules .~ rs'
  where
    rs' = potentialSrc <> inlined
    rs  = dp ^. dpRules
    potentialSrc = filter (isGround dp) rs
    inlined = do
      tgt <- refreshRule "T_" <$> rs
      let ttl = tgt ^. ruleTail
      let fil (x,_) = isPredicate x && isIDBFact dp x
      let tlPreds = filter fil $ U.contexts ttl
      (p,mut) <- maybeToList $ head <$> nonEmpty tlPreds
      let res
            | null tlPreds = return tgt
            | otherwise = 
                do
                  src <- findRules dp $ p ^. predName
                  let shd = src ^. ruleHead
                  let stl = src ^. ruleTail
                  let subst = unify shd p
                  let subster x = applySubst x (tgt & ruleTail .~ mut stl)
                  maybeToList $ subster <$> subst
      res

-- Removes duplicate terms from AND operations at the root expression
simplifyAnds :: Expr -> Expr
simplifyAnds x = foldl' eAnd h t
  where
    (h:t) = L.nub . filter (not . isAnd) $ simplifyAnds' x

simplifyAnds' :: Expr -> [Expr]
simplifyAnds' (And _ x y) = simplifyAnds' x <> simplifyAnds' y
simplifyAnds' x = [x]

isAnd :: Expr -> Bool
isAnd And{} = True
isAnd _     = False

isPredicate :: Expr -> Bool
isPredicate Pred{} = True
isPredicate _      = False

isGround :: DatalogProgram -> Rule -> Bool
isGround dp r = all (not . isIDBFact dp) . U.universe $ r ^. ruleTail

-- rewrite the assignments and look for contradictions
checkConsistency :: (MonadIO m) => Rule -> m Bool
checkConsistency r = Solve.checkSat $ r ^. ruleTail . to andsToList

-- try whether the expressions have a more compact solution
simplifySat :: (MonadFail m, MonadIO m) => Rule -> m Rule
simplifySat r = 
  do
    let vars = args r
    let e = r ^. ruleTail
    newTails@(h:t) <- (Solve.extractSatSolution vars . andsToList) e
    let newTail = if null newTails then constBool False else foldr eAnd h t
    return $ r & ruleTail .~ newTail

simplifyRules :: [Rule] -> [Rule]
simplifyRules rs = map simplifyRule rs

simplifyRule :: Rule -> Rule
simplifyRule r =
    let rName = ruleName r in
    let rBody = simplifyAnds' $ r ^. ruleTail in
    let rArgs = args r in
    let (boundedVars, freeVars) = L.partition (\z -> z ^. annotation ^. annBound) rArgs in
    let boundedVarNames = concat $ map varNames boundedVars in
    let freeVarNames = concat $ map varNames freeVars in
    let newRuleBody = simplify rBody (boundedVarNames, freeVarNames) in
    let (h:t) = ordNub . filter (not . isAnd) $ newRuleBody in
    let newRuleTail' = foldr eAnd h t in
    --trace ("before: " ++ show (pretty r)) $
    --trace ("after: " ++ show (pretty (rule rName rArgs newRuleTail))) $
    --trace "=====" $
    rule rName rArgs newRuleTail'

-- | Removes facts that always evaluate to False
removeFalseFacts :: [Rule] -> [Rule]
removeFalseFacts = filter (\x -> x ^. ruleTail /= constBool False)

