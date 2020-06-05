module DBClause 
  ( DBClause
  , dbClause
  ) where

import Data.Text.Prettyprint.Doc

import Expr

data DBClause = DBClause String [Expr]
  deriving (Show)

instance Pretty DBClause where
  pretty = pretty . show

dbClause :: String -> [Expr] -> DBClause
dbClause = DBClause

