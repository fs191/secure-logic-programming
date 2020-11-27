{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Translator.SemanticsChecker (checkSemantics) where

import Relude

import Control.Lens
import Control.Monad.Writer

import qualified Data.Generics.Uniplate.Data as U
import qualified Data.List as L

import Expr
import DatalogProgram
import ErrorMsg
import Rule
import Translator.Adornment

-- | Checks the code for errors that can be blamed on the user.
checkSemantics 
  :: DatalogProgram 
  -> ([CompilerException], Bool)
checkSemantics dp = 
  do
    --let adorned = adornProgram dp
    --let typed = typeInference adorned
    --void . traverse checkTyping $ U.universeBi typed
    let attrs = dp ^. dpDBClauses . predArgs
    -- Ensure that there are no duplicate attributes
    andM
      [ checkDBDuplicates dp
      -- , checkDuplicates attrs
      , checkPredicates dp
      , checkSinglePattern dp
      , checkDuplicateArgs dp
      ]

checkDBDuplicates 
  :: (MonadWriter [CompilerException] m) 
  => DatalogProgram 
  -> m Bool
checkDBDuplicates dp = andM $
  do
    idb <- dp ^.. dpRules . folded
    edb <- dp ^.. dpDBClauses
    return $ if ruleName idb == view predName edb
      then do
        tell [DBNameClash (idb ^. ruleHead) edb]
        return False
      else return True

checkDuplicates 
  :: (MonadWriter [CompilerException] m) 
  => [Expr] 
  -> m Bool
checkDuplicates exprs = andM $
  do
    i <- [0..length exprs - 1]
    j <- [i+1..length exprs - 1]
    return $ case (exprs !!? i, exprs !!? j) of
      (Just x, Just y) -> if _varName x == _varName y
                            then do
                              tell [MultipleAttributeDeclarations x y]
                              return False
                            else do
                              return True
      _                -> return True

-- | Check whether each predicate can be found in either EDB or IDB.
-- Only the name and number of arguments are considered.
checkPredicates 
  :: (MonadWriter [CompilerException] m)
  => DatalogProgram 
  -> m Bool
checkPredicates dp = 
  do
    let toNameArgNum p = fromMaybe (error "Expected a predicate") $
          do
            n <- p ^? predName
            a <- p ^? predArgs . to length
            return (n, a)
    let preds' = [p | p@Pred{} <- U.universeBi $ dp ^.. dpRules . folded . ruleTail]
        goalPreds = [dp ^. dpGoal]
        preds  = toNameArgNum <$> (preds' <> goalPreds)
    let idbRules = toNameArgNum <$> dp ^.. dpRules . folded . ruleHead
    let edbRules = zip (dp ^.. dpDBClauses . predName) (dp ^.. dpDBClauses . predArgs . to length)
    let rules = idbRules <> edbRules
    case L.findIndex (`notElem` rules) preds of
      Just i  ->
        do
          let culprit = fromMaybe (dp ^. dpGoal) $ preds' !!? i
          tell [UndefinedPredicate culprit]
          return False
      Nothing -> return True

checkSinglePattern 
  :: (MonadWriter [CompilerException] m)
  => DatalogProgram 
  -> m Bool
checkSinglePattern p =
  do
    let as = ordNub ((\(Adornable r bp e) -> (ruleName r, bp, e)) <$> allAdornables p)
    let f a@(n1, bp1, _) b@(n2, bp2, _)
          | n1 == n2 && 
            bp1 /= bp2 = tell [MultipleBindingPatterns a b]
          | otherwise = return ()
    sequence_ $
      do
        i <- [0..length as - 1]
        j <- [i+1..length as - 1]
        let err = error "This should never happen"
        return . fromMaybe err $ f <$> (as !!? i) <*> (as !!? j)
    return True

checkDuplicateArgs 
  :: (MonadWriter [CompilerException] m)
  => DatalogProgram 
  -> m Bool
checkDuplicateArgs dp =
  do
    let checkPred p@Pred{} = 
          if as /= ordNub as
            then do
              tell [DuplicateArguments p]
              return False
            else return True
          where as = p ^.. predArgs . folded . varName
        checkPred _ = return True
        preds :: [Expr]
        preds = dp ^.. dpRules . folded . ruleTail . to andsToList . folded
    andM $ checkPred <$> preds

