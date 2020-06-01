{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.SecreC
  ( defaultHeader
  , defaultGoal
  , function
  , struct
  , var
  ) where

---------------------------------------------------------
-- SecreC pretty printer
---------------------------------------------------------

import Data.Text (Text)
import Data.Text.Prettyprint.Doc

import Expr

newtype SCProgram = SCProgram
  { _pStatements :: [TopStatement]
  }
  deriving (Semigroup, Monoid)

instance Pretty SCProgram where
  pretty p = hsep $ punctuate semi $ pretty <$> _pStatements p

data SCKind
  = SCShared3pKind

instance Pretty SCKind where
  pretty SCShared3pKind = "shared3p"

data SCDomain
  = SCShared3p

instance Pretty SCDomain where
  pretty SCShared3p = "pd_shared3p"

data SCType
  = SCUInt32

instance Pretty SCType where
  pretty SCUInt32 = "uint32"

-- | Top-level statements
data TopStatement
  -- Function declaration
  = Funct FunctionDecl 
  -- Struct declaration
  | Struct StructDecl
  -- Import statement
  | Import String
  -- SecreC domain statement
  | Domain SCDomain SCKind
  -- Empty line
  | Empty

instance Pretty TopStatement where
  pretty (Funct f)    = pretty f
  pretty (Struct s)   = pretty s
  pretty (Import s)   = "import" <+> pretty s
  pretty (Domain d k) = "domain" <+> pretty d <+> pretty k

-- | Statements with a return type
-- TODO use GADTs to express the return type
data Statement 
  = Comment String
  -- Variable declaration
  | VarDecl SCVar
  -- Function call
  | FunCall String [Expr SCVar]

instance Pretty Statement where
  pretty (Comment t) = "//" <> pretty t
  pretty (VarDecl v) = pretty v <> semi
  pretty (FunCall n pars) = pretty n <> pars' <> semi
    where pars' = tupled $ pretty <$> pars

data SCVar = SCVar
  { _vdDomain :: SCDomain
  , _vdType   :: SCType
  , _vdName   :: Text
  }

instance Pretty SCVar where
  pretty v = k <+> t <+> n
    where
      k = pretty $ _vdDomain v
      t = pretty $ _vdType v
      n = pretty $ _vdName v

data FunctionDecl = FunctionDecl
  { _fdReturnType :: Maybe SCType
  , _fdName       :: Text
  , _fdParams     :: [SCVar]
  , _fdBody       :: [Statement]
  }

instance Pretty FunctionDecl where
  pretty fd = vsep
    [ rt <+> n <+> pars
    , lbracket
    , indent 4 body
    , rbracket
    ]
    where
      n    = pretty $ _fdName fd
      pars = tupled $ pretty <$> _fdParams fd
      body = vsep $ pretty <$> _fdBody fd
      rt   = case _fdReturnType fd of
        Just x  -> pretty x
        Nothing -> "void"

data StructDecl = StructDecl
  { _sdName    :: Text
  , _sdMembers :: [SCVar]
  }

instance Pretty StructDecl where
  pretty sd = vsep
    [ "struct" <+> n
    , lbracket
    , vsep ms
    , rbracket
    ]
    where 
      n  = pretty $ _sdName sd
      ms = punctuate semi $ pretty <$> _sdMembers sd

defaultHeader :: SCProgram
defaultHeader = program
  [ Import "stdlib"
  , Import "shared3p"
  , Import "shared3p_string"
  , Import "shared3p_table_database"
  , Import "table_database"
  , Empty
  , Import "lp_essentials"
  , Empty
  , Domain SCShared3p SCShared3pKind
  , Empty
  ]

defaultGoal :: FunctionDecl
defaultGoal = function Nothing "main" [] $
  [ VarDecl $ var SCShared3p SCUInt32 "dummy"
  , Comment "TODO: state your own goal here"
  , FunCall "publish" 
      [ ConstStr "NOTICE: no goal specified in the main function"
      , ConstBool False
      ]
  ]

function :: Maybe SCType -> Text -> [SCVar] -> [Statement] -> FunctionDecl
function = FunctionDecl

struct :: Text -> [SCVar] -> StructDecl
struct = StructDecl

var :: SCDomain -> SCType -> Text -> SCVar
var = SCVar

program :: [TopStatement] -> SCProgram
program = SCProgram

