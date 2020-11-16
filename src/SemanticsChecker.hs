{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SemanticsChecker (checkSemantics) where

import Relude

import Control.Lens
import Control.Monad.Writer

import qualified Data.Generics.Uniplate.Data as U
import Data.List (findIndex)

import DBClause
import Annotation
import Expr
import DatalogProgram
import ErrorMsg
import Rule
import Adornment

-- | Checks the code for errors that can be blamed on the user.
checkSemantics 
  :: DatalogProgram 
  -> ([CompilerException], Bool)
checkSemantics dp = 
  do
    --let adorned = adornProgram dp
    --let typed = typeInference adorned
    --void . traverse checkTyping $ U.universeBi typed
    let attrs = dp ^. dpDBClauses . to DBClause.vars
    -- Ensure that there are no duplicate attributes
    andM
      [ checkDuplicates attrs
      , checkPredicates dp
      , checkSinglePattern dp
      ]

checkDuplicates 
  :: (MonadWriter [CompilerException] m) 
  => [Expr] 
  -> m Bool
checkDuplicates exprs = andM $
  do
    i <- [0..length exprs - 1]
    j <- [i+1..length exprs - 1]
    return $ case (exprs !!? i, exprs !!? j) of
      (Just x, Just y) -> if _attrName x == _attrName y
                            then do
                              tell [MultipleAttributeDeclarations x y]
                              return False
                            else do
                              return True
      _                -> return True

checkTyping 
  :: (MonadWriter [CompilerException] m) 
  => Expr 
  -> m Bool
checkTyping And{} = return True
checkTyping Or{} = return True
checkTyping e =
  if e ^. annotation . typing . to isTyped
    then return True
    else do
      tell [TypeInferenceFailed e]
      return False
  where
    pos = e ^? annotation . srcPos . _Just

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
    let edbRules = zip (dp ^.. dpDBClauses . to name) (dp ^.. dpDBClauses . to (length . vars))
    let rules = idbRules <> edbRules
    case findIndex (flip notElem rules) preds of
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
    let as = ordNub $ (\(Adornable r bp e) -> (ruleName r, bp, e)) <$> allAdornables p
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

