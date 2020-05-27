{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module DBClause 
  ( DBClause, DBVar(..) -- TODO Do not export constructors for DBVar
  , dbClause, free, bound
  , isFree
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
  = Bound DomainType DataType String
  | Free String
  deriving (Ord, Eq, Show, Data, Typeable)


data DataType   = VarBool | VarNum | VarText | Unknown
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
  name   (Bound _ _ n) = n
  name   (Free n) = n
  rename n' (Bound x y _) = Bound x y n'
  rename n (Free _) = Free n

instance Pretty DBVar where
  pretty (Free n)          = pretty n
  pretty (Bound dom dat n) = pretty n <+> ":" <+> pretty dom <+> pretty dat

instance Pretty DataType where
  pretty VarBool = "bool"
  pretty VarText = "string"
  pretty VarNum  = "int"
  pretty Unknown = "??"

instance Pretty DomainType where
  pretty Private = "private"
  pretty Public  = "public"

free :: String -> DBVar
free = Free

bound :: DomainType -> DataType -> String -> DBVar
bound = Bound

isFree :: DBVar -> Bool
isFree (Free _) = True
isFree _        = False

dataType :: DBVar -> Maybe DataType
dataType (Bound _ dt _) = Just dt
dataType _              = Nothing

varName :: DBVar -> String
varName (Free n) = n
varName (Bound _ _ n) = n

dbClause :: String -> [DBVar] -> DBClause
dbClause = DBClause

