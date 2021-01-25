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
  , mainFun
  ) where

import Relude

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
  | Import Text
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
  = Comment Text
  -- Variable declaration
  | VarDecl SCVar
  -- Variable assignment
  | VarAsgn Text SCExpr
  -- Variable initialization
  | VarInit SCVar SCExpr
  -- Function call
  | FunCall Text [SCExpr]
  -- Return Statement
  | Return SCExpr
  -- We decide to leave empty rows inside functions as well (for better readability)
  | SCEmpty

instance Pretty Statement where
  pretty (Comment t)      = "//" <> pretty t
  pretty (VarDecl v)      = pretty v <> semi
  pretty (VarAsgn v e)    = pretty v <+> "=" <+> pretty e <> semi
  pretty (VarInit v e)    = pretty v <+> "=" <+> pretty e <> semi
  pretty (Return e)       = "return" <+> pretty e <> semi
  pretty SCEmpty          = ""
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
  { _fdTemplate   :: SCTemplate
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
      rt   = maybe "void" pretty $ _fdReturnType fd

data StructDecl = StructDecl
  { _sdTemplate   :: SCTemplate
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
      ms = pretty <$> map VarDecl (_sdMembers sd)

mainFun :: [Statement] -> FunctionDecl
mainFun = FunctionDecl (SCTemplateDecl Nothing) Nothing "main" []

