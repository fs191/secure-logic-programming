{-# LANGUAGE FlexibleContexts #-}
module SemanticsChecker (checkSemantics) where

import Relude

import Control.Lens

import qualified Data.Generics.Uniplate.Data as U
import Data.List (findIndex)

import DBClause
import Annotation
import Expr
import DatalogProgram
import ErrorMsg
import Rule

-- | Checks the code for errors that can be blamed on the user.
checkSemantics 
  :: DatalogProgram 
  -> Either CompilerException ()
checkSemantics dp = 
  do
    --let adorned = adornProgram dp
    --let typed = typeInference adorned
    --void . traverse checkTyping $ U.universeBi typed
    let attrs = dp ^. dpDBClauses . to DBClause.vars
    -- Ensure that there are no duplicate attributes
    checkDuplicates attrs
    checkPredicates dp
    return ()

checkDuplicates :: [Expr] -> Either CompilerException ()
checkDuplicates exprs = sequence_ $
  do
    i <- [0..length exprs - 1]
    j <- [i+1..length exprs - 1]
    case (exprs !!? i, exprs !!? j) of
      (Just x, Just y) -> if _attrName x == _attrName y
                            then return $ Left err
                            else return $ Right ()
        where err = CompilerException (MultipleAttributeDeclarations $ _attrName x) (y ^. annotation . srcPos)
      _                -> return $ Right ()

checkTyping :: Expr -> Either CompilerException ()
checkTyping And{} = return ()
checkTyping Or{} = return ()
checkTyping e =
  if e ^. annotation . typing . to isTyped
    then Right ()
    else Left $ CompilerException (TypeInferenceFailed e) pos
  where
    pos = e ^? annotation . srcPos . _Just

-- | Check whether each predicate can be found in either EDB or IDB.
-- Only the name and number of arguments are considered.
checkPredicates :: DatalogProgram -> Either CompilerException ()
checkPredicates dp = 
  do
    let toNameArgNum p = fromMaybe (error "Expected a predicate") $
          do
            n <- p ^? predName
            a <- p ^? predArgs . to length
            return (n, a)
    let preds' = [p | p@Pred{} <- U.universeBi $ dp ^.. dpRules . folded . ruleTail]
        preds  = toNameArgNum <$> preds'
    let idbRules = toNameArgNum <$> dp ^.. dpRules . folded . ruleHead
    let edbRules = zip (dp ^.. dpDBClauses . to name) (dp ^.. dpDBClauses . to (length . vars))
    let rules = idbRules <> edbRules
    case findIndex (flip notElem rules) preds of
      Just i  -> Left $ CompilerException (UndefinedPredicate culprit) (culprit ^. annotation . srcPos)
        where culprit = fromMaybe undefined $ preds' !!? i
      Nothing -> Right ()
