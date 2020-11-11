{-# LANGUAGE ScopedTypeVariables #-}

module Transform
  ( deriveAllGroundRules
  ) where

---------------------------------------------------------
---- Transformation of a Datalog script
----   to intermediate representation
---------------------------------------------------------

import Relude

import Data.Generics.Uniplate.Operations as U
import Data.Text.Prettyprint.Doc

import qualified Data.List as L

import Control.Lens

import Rule
import Expr
import Simplify
import Solve
import Substitution
import DatalogProgram
import Annotation

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
        pl0 <- dp & id %~ inlineOnceBFS
                 & dpRules . traversed %~ refreshRule "X_"
                 & dpRules %%~ filterM checkConsistency
        pl  <- pl0 & dpRules . traversed %%~ simplifySat
        return $ pl & dpRules . traversed . ruleTail %~ simplifyAnds
                    & dpRules %~ simplifyRules
                    & dpRules %~ removeFalseFacts
                    & dpRules %~ L.nub

loopPipeline :: Int -> (DatalogProgram -> IO DatalogProgram) -> DatalogProgram -> IO DatalogProgram
loopPipeline n pipeline dp = do
    dp' <- pipeline dp
    let groundRulesBefore = filter (isGround dp ) (dp  ^. dpRules)

    if (length groundRulesBefore >= n) || (dp == dp') then do
        return dp
    else do
        loopPipeline n pipeline dp'

-- | Tries to unify the first predicate in each rule body with an appropriate rule using Breadth-First-Search strategy
inlineOnceBFS :: DatalogProgram -> DatalogProgram
inlineOnceBFS dp = 
  trace "====" $
  traceShow (pretty dp) $
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
    (potentialSrc, potentialTgt) = L.partition (isGround dp) rs

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
            let (p,mut) = fromMaybe undefined $ viaNonEmpty head tlPreds in
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
                    let newTail = case nonEmpty newTails of
                          Just x  -> foldr eAnd (head x) (tail x)
                          Nothing -> constFalse
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

    let newRuleTail  = fromMaybe undefined . nonEmpty . ordNub . filter (not . isAnd) $ newRuleBody in
    let newRuleTail' = foldr eAnd (head newRuleTail) (tail newRuleTail) in
    --trace ("before: " ++ show (pretty r)) $
    --trace ("after: " ++ show (pretty (rule rName rArgs newRuleTail))) $
    --trace "=====" $
    rule rName rArgs newRuleTail'

-- | Removes facts that always evaluate to False
removeFalseFacts :: [Rule] -> [Rule]
removeFalseFacts = filter (\x -> x ^. ruleTail /= constBool False)

