module DBClause 
  ( DBClause
  , dbClause
  , name, vars
  ) where

import Relude

import Data.Text.Prettyprint.Doc

import Expr
import ExprPretty

data DBClause = DBClause Text [Expr]
  deriving (Show)

instance Pretty DBClause where
  pretty (DBClause n vs) = "!" <> pretty n <> vars'
    where vars' = tupled $ prettyFull <$> vs

dbClause :: Text -> [Expr] -> DBClause
dbClause = DBClause

name :: DBClause -> Text
name (DBClause n _) = n

vars :: DBClause -> [Expr]
vars (DBClause _ vs) = vs

