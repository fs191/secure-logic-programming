{-# LANGUAGE FlexibleContexts #-}
module SemanticsChecker (checkSemantics) where

import Relude

import Control.Lens
import qualified Data.Generics.Uniplate.Data as U

import Annotation
import Adornment
import Expr
import DatalogProgram
import TypeInference
import ErrorMsg

-- | Checks the code for errors that can be blamed on the user.
checkSemantics 
  :: DatalogProgram 
  -> Either CompilerException ()
checkSemantics dp = 
  do
    let adorned = adornProgram dp
    let typed = typeInference adorned
    void . traverse checkTyping $ U.universeBi typed
    return ()

checkTyping :: Expr -> Either CompilerException ()
checkTyping e =
  if e ^. annotation . typing . to isTyped
    then Right ()
    else Left $ CompilerException (TypeInferenceFailed e) pos
  where
    pos = e ^? annotation . srcPos . _Just . _1

