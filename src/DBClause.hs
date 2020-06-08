{-# LANGUAGE OverloadedStrings #-}

module DBClause 
  ( DBClause
  , dbClause
  , name, vars
  ) where

import Data.Text.Prettyprint.Doc

import Expr

data DBClause = DBClause String [Expr]
  deriving (Show)

instance Pretty DBClause where
  pretty (DBClause n vs) = "!" <> pretty n <> vars'
    where vars' = tupled $ pretty <$> vs

dbClause :: String -> [Expr] -> DBClause
dbClause = DBClause

name :: DBClause -> String
name (DBClause n _) = n

vars :: DBClause -> [Expr]
vars (DBClause _ vs) = vs

