{-# LANGUAGE ScopedTypeVariables #-}

module Transform
  ( deriveAllGroundRules
  ) where

---------------------------------------------------------
---- Transformation of a Datalog script
----   to intermediate representation
---------------------------------------------------------

import Data.Generics.Uniplate.Operations as U
import qualified Data.List.Safe as S
import Data.Maybe

import Control.Lens as L
import Control.Monad

import Rule
import Expr
import Solve
import Substitution
import DatalogProgram

import Data.Text.Prettyprint.Doc
import Debug.Trace

-- | Generates all possible ground rules for `n` iterations by inlining
-- predicates with matching rules. Each predicate gets inlined once per
-- iteration.
deriveAllGroundRules :: Int -> DatalogProgram -> IO DatalogProgram
deriveAllGroundRules n program = f program'
  where
    -- Input program but db clauses are converted to rules
    program' = program
    f :: DatalogProgram -> IO DatalogProgram 
    f rs = loopPipeline n pipeline rs

    pipeline :: DatalogProgram -> IO DatalogProgram
    pipeline dp = 
      do
        pl0 <- dp & id %~ inlineOnceGr
                 & dpRules . traversed %~ refreshRule "X_"
                 & dpRules %%~ filterM checkConsistency
        pl  <- pl0 & dpRules . traversed %%~ simplifySat
        return $ pl & dpRules . traversed . ruleTail %~ simplifyAnds
                    & dpRules %~ removeFalseFacts
                    & dpRules %~ removeDuplicateFacts

loopPipeline :: Int -> (DatalogProgram -> IO DatalogProgram) -> DatalogProgram -> IO DatalogProgram
loopPipeline n pipeline dp = do
    dp' <- pipeline dp
    let groundRulesBefore = filter (isGround dp ) (dp  ^. dpRules)
    let groundRulesAfter  = filter (isGround dp') (dp' ^. dpRules)

    -- TODO the following condition is correct only for the Gr strategy
    if (length groundRulesBefore >= n) || (length groundRulesBefore == length groundRulesAfter) then do
        return dp

    else do
        loopPipeline n pipeline dp'

-- | Tries to unify the first predicate in each rule body with an appropriate rule using Breadth-First-Search strategy
inlineOnceBFS :: DatalogProgram -> DatalogProgram
inlineOnceBFS dp = 
  --trace "====" $
  --trace (show (length (dp ^. dpRules))) $
  --trace (show (length (filter (isGround dp) (dp ^. dpRules)))) $
  dp & dpRules .~ rs'


  where
    rs' = potentialSrc <> inlined
    rs  = dp ^. dpRules
    potentialSrc = filter (isGround dp) rs
    inlined = do
      tgt <- refreshRule "T_" <$> rs
      let ttl = tgt ^. ruleTail
      let fil (x,_) = isPredicate x && isIDBFact dp x
      let tlPreds = filter fil $ U.contexts ttl
      (p,mut) <- S.head tlPreds
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

-- | Tries to unify the first predicate in each rule body with an appropriate rule using Depth-First-Search strategy
inlineOnceDFS :: DatalogProgram -> DatalogProgram
inlineOnceDFS dp = 
  --trace "====" $
  --trace (show (length (dp ^. dpRules))) $
  --trace (show (length potentialSrc)) $
  dp & dpRules .~ rs'

  where
    rs' = potentialSrc <> inlined <> tail potentialTgt
    rs  = dp ^. dpRules
    (potentialSrc, potentialTgt') = S.partition (isGround dp) rs

    -- we start from the rules that contain the least amount of IDB predicates and hence bring us ground rules faster
    potentialTgt = S.sortOn (uncertaintyDegree dp) potentialTgt'

    inlined = do
      tgt <- refreshRule "T_" <$> S.head potentialTgt
      let ttl = tgt ^. ruleTail
      let fil (x,_) = isPredicate x && isIDBFact dp x
      let tlPreds = filter fil $ U.contexts ttl
      (p,mut) <- S.head tlPreds
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

-- | Tries to unify the first predicate in each rule body with an appropriate rule using full-ground strategy
inlineOnceGr :: DatalogProgram -> DatalogProgram
inlineOnceGr dp = 
  --trace "====" $
  --trace (show (length rs)) $
  --trace (show (pretty potentialSrc)) $
  --trace (show (pretty inlined)) $
  --trace (show (length potentialTgt)) $
  dp & dpRules .~ rs'


  where
    rs' = potentialSrc <> inlined <> potentialTgt
    rs  = dp ^. dpRules
    (potentialSrc, potentialTgt) = S.partition (isGround dp) rs

    fil (x,_) = isPredicate x && isIDBFact dp x

    inlined = do
      tgt <- potentialTgt
      substututeAllIDB tgt

    substututeAllIDB :: Rule -> [Rule]
    substututeAllIDB tgt' =
        let tgt = refreshRule "T_" tgt' in
        let ttl = tgt ^. ruleTail in
        let tlPreds = filter fil $ U.contexts ttl in
        if length tlPreds == 0 then [tgt]
        else
            let (p,mut) = head tlPreds in
            let tgts = do                
                     src <- filter (isGround dp) $ findRules dp $ p ^. predName
                     let shd = src ^. ruleHead
                     let stl = src ^. ruleTail
                     let subst = unify shd p
                     let subster x = applySubst x (tgt & ruleTail .~ mut stl)
                     maybeToList $ subster <$> subst
            in
            --trace ("\n" ++ show (pretty p) ++ "\n" ++ show (pretty tgt) ++ "\n" ++ show (pretty tgts)) $
            concat $ map substututeAllIDB tgts

-- Removes duplicate terms from AND operations at the root expression
simplifyAnds :: Expr -> Expr
simplifyAnds x = foldr1 eAnd . S.nub . filter (not . isAnd) $ simplifyAnds' x

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

uncertaintyDegree :: DatalogProgram -> Rule -> Int
uncertaintyDegree dp r = length $ filter (isIDBFact dp) . U.universe $ r ^. ruleTail

-- rewrite the assignments and look for contradictions
checkConsistency :: Rule -> IO Bool
checkConsistency r = Solve.checkSat $ r ^. ruleTail . to andsToList

-- try whether the expressions have a more compact solution
simplifySat :: Rule -> IO Rule
simplifySat r = do
                    let vars = args r
                    let e = r ^. ruleTail
                    newTails <- (Solve.extractSatSolution vars . andsToList) e
                    let newTail = if newTails == [] then constBool False else foldr1 eAnd newTails
                    return $ r & ruleTail .~ newTail

-- | Removes facts that always evaluate to False
removeFalseFacts :: [Rule] -> [Rule]
removeFalseFacts = filter (\x -> x ^. ruleTail /= constBool False)

-- | Removes duplicates of facts that appear more than once
removeDuplicateFacts :: [Rule] -> [Rule]
removeDuplicateFacts = S.nub

