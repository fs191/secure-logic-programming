{-# LANGUAGE FlexibleContexts #-}
module SemanticsChecker (checkSemantics) where

import Relude

import Control.Lens

import Annotation
import Expr
import DatalogProgram
import ErrorMsg
import qualified DBClause

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
    -- TODO Ensure that all predicates are either in IDB or EDB
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

