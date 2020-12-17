{-# LANGUAGE OverloadedStrings #-}

module Language.SecreC.SCExpr 
  ( SCTemplate(..)
  , SCDomain(..), SCKind(..), SCType(..)
  , SCExpr(..)
  , angled
  ) where

import Relude hiding (show)

import Data.Text.Prettyprint.Doc

data SCTemplate
  = SCTemplateDecl (Maybe ([SCDomain], [SCType]))
  | SCTemplateUse  (Maybe ([SCDomain], [SCType]))
  deriving (Show, Eq)

instance Pretty SCTemplate where
  pretty (SCTemplateDecl Nothing) = ""
  pretty (SCTemplateUse  Nothing) = ""
  pretty (SCTemplateDecl (Just (ds,xs))) = line <> "template" <> angled (pds <> pxs)
      where pds = map ("domain" <+>) $ pretty <$> ds
            pxs = map ("type" <+>)   $ pretty <$> xs
  pretty (SCTemplateUse  (Just (ds,xs))) = angled (pds <> pxs)
      where pds = pretty <$> ds
            pxs = pretty <$> xs

data SCKind
  = SCShared3pKind

instance Pretty SCKind where
  pretty SCShared3pKind = "shared3p"

data SCDomain
  = SCShared3p | SCPublic | SCDynamic (Maybe Int)
  deriving (Show, Eq)

instance Pretty SCDomain where
  pretty SCShared3p  = "pd_shared3p"
  pretty SCPublic    = "public"
  pretty (SCDynamic Nothing)  = "D"
  pretty (SCDynamic (Just i)) = "D" <> pretty i

data SCType
  = SCUInt8
  | SCUInt16
  | SCUInt32
  | SCUInt64
  | SCXorUInt8
  | SCXorUInt16
  | SCXorUInt32
  | SCXorUInt64
  | SCInt8
  | SCInt16
  | SCInt32
  | SCInt64
  | SCFloat32
  | SCFloat64
  | SCBool
  | SCText
  | SCDynamicT (Maybe Int)
  | SCDynamicS (Maybe Int)
  | SCColumn SCDomain SCType SCType
  | SCSubst SCDomain SCType
  | SCStruct Text SCTemplate
  | SCArray Int SCType
  deriving (Show, Eq)

instance Pretty SCType where
  pretty SCFloat32   = "float"
  pretty SCFloat64   = "float64"
  pretty SCUInt8     = "uint8"
  pretty SCUInt16    = "uint16"
  pretty SCUInt32    = "uint"
  pretty SCUInt64    = "uint64"
  pretty SCXorUInt8  = "xor_uint8"
  pretty SCXorUInt16 = "xor_uint16"
  pretty SCXorUInt32 = "xor_uint32"
  pretty SCXorUInt64 = "xor_uint64"
  pretty SCInt8      = "int8"
  pretty SCInt16     = "int16"
  pretty SCInt32     = "int32"
  pretty SCInt64     = "int64"
  pretty SCBool      = "bool"
  pretty SCText    = "string"

  -- we are using two values for each string column: the true value S, and a hash T used for comparisons
  pretty (SCDynamicT Nothing)  = "T"
  pretty (SCDynamicT (Just i)) = "T" <> pretty i
  pretty (SCDynamicS Nothing)  = "S"
  pretty (SCDynamicS (Just i)) = "S" <> pretty i

  -- the column template defines its privacy type and the two types of values
  pretty (SCColumn pt dt st) = "relColumn" <> angled [pretty pt, pretty dt, pretty st]
  pretty (SCSubst pt dt)  = "subst" <> angled [pretty pt, pretty dt]
  pretty (SCStruct s t)     = pretty s <> pretty t
  pretty (SCArray n sctype) = pretty sctype <+> "[[" <> pretty n <> "]]"

-- since SecreC syntax is different from Datalog's, let us define SecreC's own expression type
-- we will only need to define pretty printing for it
data SCExpr
  = SCConstInt   Int
  | SCConstFloat Float
  | SCConstStr   Text
  | SCConstBool  Bool
  | SCConstArr   [SCExpr]
  | SCConstAny   Text
  | SCVarName    Text
  | SCNot SCExpr
  | SCNeg SCExpr
  | SCInv SCExpr
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
  | SCSum  [SCExpr]
  | SCProd [SCExpr]
  | SCOrs  [SCExpr]
  | SCAnds [SCExpr]
  | SCTypeCast SCType SCExpr
  | SCFunCall Text [SCExpr]

instance Pretty SCExpr where
  pretty (SCConstInt x)   = pretty x
  pretty (SCConstFloat x) = pretty x
  pretty (SCConstStr x)   = dquotes $ pretty x
  pretty (SCConstBool True)  = "true"
  pretty (SCConstBool False) = "false"
  pretty (SCConstArr xs)  = if not $ null xs then
                              lbrace <> (hsep . punctuate ",") (pretty <$> xs) <> rbrace
                            else
                              "reshape(0, 0)"
  pretty (SCConstAny x)   = pretty x
  pretty (SCVarName x)    = pretty x
  pretty (SCNot e)        = "!" <> parens (pretty e)
  pretty (SCNeg e)        = "-" <> parens (pretty e)
  pretty (SCInv e)        = "1 / " <> parens (pretty e)
  pretty (SCDiv x y)      = parens (pretty x) <+> "/" <+> parens (pretty y)
  pretty (SCSub x y)      = parens (pretty x) <+> "-" <+> parens (pretty y)
  pretty (SCLt x y)       = parens (pretty x) <+> "<" <+> parens (pretty y)
  pretty (SCLe x y)       = parens (pretty x) <+> "<=" <+> parens (pretty y)
  pretty (SCEq x y)       = parens (pretty x) <+> "==" <+> parens (pretty y)
  pretty (SCGt x y)       = parens (pretty x) <+> ">" <+> parens (pretty y)
  pretty (SCGe x y)       = parens (pretty x) <+> ">=" <+> parens (pretty y)
  pretty (SCMul x y)      = parens (pretty x) <+> "*" <+> parens (pretty y)
  pretty (SCAdd x y)      = parens (pretty x) <+> "+" <+> parens (pretty y)
  pretty (SCOr x y)       = parens (pretty x) <+> "|" <+> parens (pretty y)
  pretty (SCAnd x y)      = parens (pretty x) <+> "&" <+> parens (pretty y)
  pretty (SCSum xs)       = hsep . punctuate "+" $ pretty <$> xs
  pretty (SCProd xs)      = hsep . punctuate "*" $ pretty <$> xs
  pretty (SCAnds xs)      = hsep . punctuate "&" $ pretty <$> xs
  pretty (SCOrs xs)       = hsep . punctuate "|" $ pretty <$> xs
  pretty (SCTypeCast t x) = parens (pretty t) <> pretty x
  pretty (SCFunCall f xs) = pretty f <> xs'
    where xs' = tupled $ pretty <$> xs

-- Utils

angled :: [Doc ann] -> Doc ann
angled prettyContent = encloseSep (langle <> space) (rangle <> space) comma prettyContent

