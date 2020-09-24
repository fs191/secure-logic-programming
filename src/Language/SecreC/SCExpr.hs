{-# LANGUAGE OverloadedStrings #-}

module Language.SecreC.SCExpr 
  ( SCTemplate(..)
  , SCDomain(..), SCKind(..), SCType(..)
  , SCExpr(..)
  , dynDomainToType
  , angled
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc

data SCTemplate
  = SCTemplateDecl ([SCDomain], [SCType])
  | SCTemplateUse  ([SCDomain], [SCType])
  deriving (Show, Eq)

instance Pretty SCTemplate where
  pretty (SCTemplateDecl (ds,xs)) = line <> "template" <> angled (pds ++ pxs)
      where pds = map ("domain" <+>) $ pretty <$> ds
            pxs = map ("type" <+>)   $ pretty <$> xs
  pretty (SCTemplateUse  (ds,xs)) = angled (pds ++ pxs)
      where pds = pretty <$> ds
            pxs = pretty <$> xs

data SCKind
  = SCShared3pKind

instance Pretty SCKind where
  pretty SCShared3pKind = "shared3p"

data SCDomain
  = SCShared3p | SCPublic | SCDynamic Text
  deriving (Show, Eq)

instance Pretty SCDomain where
  pretty SCShared3p  = "pd_shared3p"
  pretty SCPublic    = "public"
  pretty (SCDynamic n) = pretty n

data SCType
  = SCUInt32
  | SCUInt
  | SCUInt8
  | SCXorUInt32
  | SCXorUInt8
  | SCInt32
  | SCFloat32
  | SCBool
  | SCString
  | SCDynamicT Text
  | SCColumn SCDomain SCType SCType
  | SCSubst SCDomain SCType
  | SCStruct Text (Maybe SCTemplate)
  | SCArray Int SCType
  deriving (Show, Eq)

instance Pretty SCType where
  pretty SCFloat32   = "float32"
  pretty SCUInt32    = "uint32"
  pretty SCUInt      = "uint"
  pretty SCUInt8     = "uint8"
  pretty SCXorUInt32 = "xor_uint32"
  pretty SCXorUInt8  = "xor_uint8"
  pretty SCInt32     = "int32"
  pretty SCBool      = "bool"
  pretty SCString    = "string"

  -- we are using two values for each string column: the true value S, and a hash T used for comparisons
  pretty (SCDynamicT i) = pretty i

  -- the column template defines its privacy type and the two types of values
  pretty (SCColumn pt dt st) = "relColumn" <> angled [pretty pt, pretty dt, pretty st]
  pretty (SCSubst pt dt)  = "subst" <> angled [pretty pt, pretty dt]
  pretty (SCStruct s (Just t))     = pretty s <> pretty t
  pretty (SCStruct s Nothing)      = pretty s
  pretty (SCArray n sctype) = pretty sctype <+> "[[" <> pretty n <> "]]"


-- since SecreC syntax is different from Datalog's, let us define SecreC's own expression type
-- we will only need to define pretty printing for it
data SCExpr
  = SCConstInt    Int
  | SCConstFloat  Float
  | SCConstStr    String
  | SCConstBool   Bool
  | SCConstArr    [SCExpr]
  | SCConstAny    String
  | SCVarName     Text
  | SCFieldAccess SCExpr Text
  | SCNot SCExpr
  | SCNeg SCExpr
  | SCDiv SCExpr SCExpr
  | SCSub SCExpr SCExpr
  | SCLt  SCExpr SCExpr
  | SCLe  SCExpr SCExpr
  | SCEq  SCExpr SCExpr
  | SCGt  SCExpr SCExpr
  | SCGe  SCExpr SCExpr
  | SCMul SCExpr SCExpr
  | SCAdd SCExpr SCExpr
  | SCAnd SCExpr SCExpr
  | SCOr  SCExpr SCExpr
  | SCTypeCast SCType SCExpr
  | SCFunCall Text [SCExpr]
  | SCAsgn SCExpr SCExpr

instance Pretty SCExpr where
  pretty (SCConstInt x)   = pretty x
  pretty (SCConstFloat x) = pretty x
  pretty (SCConstStr x)   = dquotes $ pretty x
  pretty (SCConstBool True)  = "true"
  pretty (SCConstBool False) = "false"
  pretty (SCConstArr xs)  = 
    if length xs > 0 then
      lbrace <> (hsep . punctuate ",") (pretty <$> xs) <> rbrace
    else
      pretty $ SCFunCall "reshape" [SCConstInt 0, SCConstInt 0]
  pretty (SCConstAny x)      = pretty x
  pretty (SCVarName x)       = pretty x
  pretty (SCFieldAccess x y) = pretty x <> "." <> pretty y
  pretty (SCNot e)           = "!" <> parens (pretty e)
  pretty (SCNeg e)           = "-" <> parens (pretty e)
  pretty (SCDiv x y)         = parens (pretty x) <+> "/"  <+> parens (pretty y)
  pretty (SCSub x y)         = parens (pretty x) <+> "-"  <+> parens (pretty y)
  pretty (SCLt x y)          = parens (pretty x) <+> "<"  <+> parens (pretty y)
  pretty (SCLe x y)          = parens (pretty x) <+> "<=" <+> parens (pretty y)
  pretty (SCEq x y)          = parens (pretty x) <+> "==" <+> parens (pretty y)
  pretty (SCGt x y)          = parens (pretty x) <+> ">"  <+> parens (pretty y)
  pretty (SCGe x y)          = parens (pretty x) <+> ">=" <+> parens (pretty y)
  pretty (SCMul x y)         = parens (pretty x) <+> "*"  <+> parens (pretty y)
  pretty (SCAdd x y)         = parens (pretty x) <+> "+"  <+> parens (pretty y)
  pretty (SCOr x y)          = parens (pretty x) <+> "|"  <+> parens (pretty y)
  pretty (SCAnd x y)         = parens (pretty x) <+> "&"  <+> parens (pretty y)
  pretty (SCTypeCast t x)    = parens $ parens (pretty t) <> pretty x
  pretty (SCFunCall f xs)    = pretty f <> (tupled $ pretty <$> xs)
  pretty (SCAsgn x y)        = pretty x <+> "=" <+> pretty y

-- Utils

angled :: [Doc ann] -> Doc ann
angled prettyContent = encloseSep (langle <> space) (rangle <> space) comma prettyContent

dynDomainToType :: SCDomain -> SCType
dynDomainToType (SCDynamic i) = SCDynamicT i
dynDomainToType x = error $ "Expected dynamic domain, got " ++ show x

