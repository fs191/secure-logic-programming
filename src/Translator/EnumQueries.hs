{-# LANGUAGE FlexibleContexts #-}
module Translator.EnumQueries 
  ( enumerateQueries
  ) where

import Relude

import Control.Lens
import Control.Monad.Except

import Data.Generics.Uniplate.Data as U

import ErrorMsg
import Annotation
import DatalogProgram
import Rule
import Expr

enumerateQueries :: (MonadError CompilerException m) 
  => DatalogProgram 
  -> m DatalogProgram
enumerateQueries dp = do
    let dp' = flip evalState (0::Int) $ dp & id %%~ U.transformBiM enumQuery
    return dp'

enumQuery :: Expr -> State Int Expr
enumQuery (Query ann _ e) =
  do
    n <- get
    modify (+1)
    return $ Query ann n e
enumQuery x = return x

