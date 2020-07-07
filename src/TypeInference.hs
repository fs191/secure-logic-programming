{-# LANGUAGE TemplateHaskell #-}
module TypeInference
  ( typeInference
  ) where

import Control.Lens
import Control.Monad.State

import Data.Map as M

import Expr
import DatalogProgram as DP
import Language.SecreC.Types ()

typeInference :: DatalogProgram -> DatalogProgram
typeInference dp = undefined

inferExpr :: Expr -> Expr
inferExpr = undefined

