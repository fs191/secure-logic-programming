{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module DBClause 
  ( DBClause, DBVar
  , dbClause, free, bound
  , dataType
  , DataType(..), DomainType(..)
  , varName
  , Named(..)
  ) where

import Data.Text.Prettyprint.Doc
import Data.Data hiding (DataType)

class Named a where
  name   :: a -> String
  rename :: String -> a -> a

-- predicate argument, together with the privacy/data type
-- here var is a database variable (not a free LP variable)
data DBVar
  = DBVar DomainType DataType String
  deriving (Ord, Eq, Show, Data, Typeable)


data DataType   = VarBool | VarNum | VarText | Auto
  deriving (Ord,Eq,Show,Data,Typeable)
data DomainType = Public  | Private
  deriving (Ord,Eq,Show,Data,Typeable)

data DBClause = DBClause 
  { _dcFunctor :: String 
  , _dcVars    :: [DBVar]
  }
  deriving (Show)

instance Pretty DBClause where
  pretty c = 
    ":- type" <>
    parens (
      (pretty $ _dcFunctor c) <>
      (parens $ hsep $ punctuate "," $ pretty <$> _dcVars c)
    )

instance Named DBVar where
  name   (DBVar _ _ n) = n
  rename n' (DBVar x y _) = DBVar x y n'

instance Pretty DBVar where
  pretty (DBVar Public Auto n) = pretty n
  pretty (DBVar dom dat n) = pretty n <+> ":" <+> pretty dom <+> pretty dat

instance Pretty DataType where
  pretty VarBool = "bool"
  pretty VarText = "string"
  pretty VarNum  = "int"
  pretty Auto    = "auto"

instance Pretty DomainType where
  pretty Private = "private"
  pretty Public  = "public"

-- | Creates a dynamically typed public variable
free :: String -> DBVar
free = DBVar Public Auto

-- | Creates a private variable with given domain and type
bound :: DomainType -> DataType -> String -> DBVar
bound = DBVar 

dataType :: DBVar -> Maybe DataType
dataType (DBVar _ dt _) = Just dt

varName :: DBVar -> String
varName (DBVar _ _ n) = n

dbClause :: String -> [DBVar] -> DBClause
dbClause = DBClause

