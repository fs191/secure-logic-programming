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
import Adornment

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
    checkSinglePattern dp
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
        where err = MultipleAttributeDeclarations $ _attrName x
      _                -> return $ Right ()

checkTyping :: Expr -> Either CompilerException ()
checkTyping And{} = return ()
checkTyping Or{} = return ()
checkTyping e =
  if e ^. annotation . typing . to isTyped
    then Right ()
    else Left $ TypeInferenceFailed e
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
      Just i  -> Left $ UndefinedPredicate culprit
        where culprit = fromMaybe undefined $ preds' !!? i
      Nothing -> Right ()

checkSinglePattern :: DatalogProgram -> Either CompilerException ()
checkSinglePattern p =
  do
    let as = allAdornables p
    let f a@(Adornable r1 bp1 _) b@(Adornable r2 bp2 _)
          | ruleName r1 == ruleName r2 && 
            bp1 /= bp2 = Left $ MultipleBindingPatterns a b
          | otherwise = Right ()
    sequence_ $
      do
        i <- [0..length as - 1]
        j <- [i+1..length as - 1]
        let err = error "This should never happen"
        return . fromMaybe err $ f <$> (as !!? i) <*> (as !!? j)

