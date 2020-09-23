{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.SecreC.SCProgram 
  ( SCProgram(..)
  , SCVar(..)
  , SCDomain, SCType
  , TopStatement(..)
  , Statement(..)
  , StructDecl(..)
  , FunctionDecl(..)
  , varToExpr
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc

import Language.SecreC.SCExpr



newtype SCProgram = SCProgram
  { _pStatements :: [TopStatement]
  }
  deriving (Semigroup, Monoid)

instance Pretty SCProgram where
  pretty p = vsep $ pretty <$> _pStatements p

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
  pretty (Import s)   = "import" <+> pretty s <> semi
  pretty (Domain d k) = "domain" <+> pretty d <+> pretty k <> semi
  pretty Empty = ""

-- | Statements with a return type
data Statement 
  = Comment String
  -- Variable declaration
  | VarDecl SCVar (Maybe SCExpr)
  -- Function call
  | ExprStmt SCExpr
  -- Return Statement
  | Return SCExpr
  -- We decide to leave empty rows inside functions as well (for better readability)
  | SCEmpty

instance Pretty Statement where
  pretty (Comment t)   = "//" <> pretty t
  pretty (VarDecl v Nothing)  = pretty v <> semi
  pretty (VarDecl v (Just e)) = pretty v <+> "=" <+> pretty e <> semi
  pretty (Return e)    = "return" <+> pretty e <> semi
  pretty (SCEmpty)     = ""
  pretty (ExprStmt e)  = pretty e <> semi

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
  { _fdTemplate   :: Maybe SCTemplate
  , _fdReturnType :: Maybe SCType
  , _fdName       :: Text
  , _fdParams     :: [SCVar]
  , _fdBody       :: [Statement]
  }

instance Pretty FunctionDecl where
  pretty fd = vsep
    [ template
    , rt <+> n <+> pars
    , lbrace
    , indent 4 (vsep body)
    , rbrace
    ]
    where
      template = pretty $ _fdTemplate fd
      n    = pretty $ _fdName fd
      pars = tupled $ pretty <$> _fdParams fd
      body = pretty <$> _fdBody fd
      rt   = case _fdReturnType fd of
        Just x  -> pretty x
        Nothing -> "void"

data StructDecl = StructDecl
  { _sdTemplate   :: Maybe SCTemplate
  , _sdName       :: Text
  , _sdMembers    :: [SCVar]
  }

instance Pretty StructDecl where
  pretty sd = vsep
    [ template
    , "struct" <+> n
    , lbrace
    , indent 4 (vsep ms)
    , rbrace
    ]
    where
      template = pretty $ _sdTemplate sd
      n  = pretty $ _sdName sd
      ms = pretty <$> map (\x -> VarDecl x Nothing) (_sdMembers sd)

varToExpr :: SCVar -> SCExpr
varToExpr = undefined

