{-# LANGUAGE DeriveAnyClass #-}

module Interpreter
  ( query, queryWithGoal
  ) where

import Control.Exception

import DatalogProgram

data InterpreterException
  = NoGoalException
  deriving (Show, Exception)

data QueryResult

query :: (Monad m) => DatalogProgram -> m QueryResult
query prog =
  case g of
    Just x  -> queryWithGoal prog x
    Nothing -> throw NoGoalException
  where g = goal prog

queryWithGoal :: (Monad m) => DatalogProgram -> Goal -> m QueryResult
queryWithGoal = undefined

