{-# LANGUAGE OverloadedStrings #-}

module DBClause 
  ( DBClause, DBVar(..) -- TODO Do not export constructors for DBVar
  , dbClause, free, bound
  , isFree
  , dataType
  , DataType(..), DomainType(..)
  , rename, varName
  ) where

import Data.Text.Prettyprint.Doc

-- predicate argument, together with the privacy/data type
-- here var is a database variable (not a free LP variable)
{-# DEPRECATED Bound "Avoid using constructors directly" #-}
{-# DEPRECATED Free "Avoid using constructors directly" #-}
data DBVar
  = Bound DomainType DataType String
  | Free String
  deriving (Ord, Eq, Show)


-- TODO Remove Unknown constructor and use Maybe instead
data DataType   = VarBool | VarNum | VarText | Unknown
  deriving (Ord,Eq,Show)
data DomainType = Public  | Private
  deriving (Ord,Eq,Show)

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

instance Pretty DBVar where
  pretty (Free n)          = pretty n
  pretty (Bound dom dat n) = pretty n <+> ":" <+> pretty dom <+> pretty dat

instance Pretty DataType where
  pretty VarBool = "bool"
  pretty VarText = "string"
  pretty VarNum  = "int"

instance Pretty DomainType where
  pretty Private = "private"
  pretty Public  = "public"

free :: String -> DBVar
free = Free

bound :: DomainType -> DataType -> String -> DBVar
bound = Bound

rename :: String -> DBVar -> DBVar
rename n (Free _) = Free n
rename n (Bound x y _) = Bound x y n

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

