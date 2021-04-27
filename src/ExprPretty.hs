{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module ExprPretty 
  ( PrettyConfig(..)
  , prettyEx
  , prettyMinimal
  , prettyFull
  ) where

import Relude

import Control.Lens hiding (List, op)

import Data.Text.Prettyprint.Doc
import qualified Data.Text as T

import Annotation
import Expr

data PrettyConfig = PrettyConfig
  { _pcShowType    :: Bool
  , _pcShowDomain  :: Bool
  , _pcShowBound   :: Bool
  , _pcShowAndAnns :: Bool
  }
makeLenses ''PrettyConfig

prettyEx :: Expr -> Reader PrettyConfig (Doc ann)
prettyEx (Var e x) = 
    (pretty x <>) <$> prettyExAnn e
prettyEx (ConstInt e x) =
    (pretty x <>) <$> prettyExAnn e
prettyEx (ConstStr e x) =
    (pretty x <>) <$> prettyExAnn e
prettyEx (ConstBool e x) =
    (pretty x <>) <$> prettyExAnn e
prettyEx (ConstFloat e x) =
    (pretty x <>) <$> prettyExAnn e
prettyEx (Attribute e x) =
    (pretty x <>) <$> prettyExAnn e
prettyEx (Hole e) = 
    ("_" <>) <$> prettyExAnn e
prettyEx (Pred e n args)  =
  do
    args' <- traverse prettyEx args
    ann <- prettyExAnn e
    return $ pretty n <> tupled args' <> ann
prettyEx (Not e x) = 
  do
    x' <- prettyEx x
    ann <- prettyExAnn e
    return $ "!" <> x' <> ann
prettyEx (Neg e x) =
  do
    x' <- prettyEx x
    ann <- prettyExAnn e
    return $ "-(" <> x' <> ")" <> ann
prettyEx (Inv e x) = 
  do
    x' <- prettyEx x
    ann <- prettyExAnn e
    return $ "1/(" <> x' <> ")" <> ann
prettyEx (Div e x y)      = prettyExBin e x y "/"
prettyEx (FDiv e x y)     = prettyExBin e x y "/"
prettyEx (Sub e x y)      = prettyExBin e x y "-"
prettyEx (Lt e x y)       = prettyExBin e x y "<"
prettyEx (Le e x y)       = prettyExBin e x y "=<"
prettyEx (Eq e x y)       = prettyExBin e x y "=:="
prettyEx (Un e x y)       = prettyExBin e x y "="
prettyEx (Is e x y)       = prettyExBin e x y "is"
prettyEx (Neq e x y)      = prettyExBin e x y "=\\="
prettyEx (Gt e x y)       = prettyExBin e x y ">"
prettyEx (Ge e x y)       = prettyExBin e x y ">="
prettyEx (Mul e x y)      = prettyExBin e x y "*"
prettyEx (Add e x y)      = prettyExBin e x y "+"
prettyEx (Pow e x y)      = prettyExBin e x y "^"
prettyEx (Or e x y)       = 
  do
    x' <- prettyEx x
    y' <- prettyEx y
    showAnn <- view pcShowAndAnns
    e' <- if showAnn then (" " <>) <$> prettyExAnn e else return ""
    return $ "(" <> x' <> ";\n" <> y' <> ")" <+> e'
prettyEx (And e x y)      =
  do
    x' <- prettyEx x
    y' <- prettyEx y
    showAnn <- view pcShowAndAnns
    e' <- if showAnn then (" " <>) <$> prettyExAnn e else return ""
    return $ x' <> ",\n" <> y' <> e'
prettyEx (List e x)       =
  do
    x' <- traverse prettyEx x
    e' <- prettyExAnn e
    return $ list x' <+> e'
prettyEx (Aggr e f p x y) = 
  do
    p' <- prettyEx p
    x' <- prettyEx x
    y' <- prettyEx y
    e' <- prettyExAnn e
    return $ hcat
      [ pretty f
      , "(" <> p' <> ", " <> x' <> "," <> y' <> ") "
      , e'
      ]
prettyEx (Query e i x) =
  do
    x' <- prettyEx x
    e' <- prettyExAnn e
    return $ "query_" <> pretty i <> "(" <> x' <> ")" <+> e'
prettyEx (Sqrt e x) = 
  do
    x' <- prettyEx x
    e' <- prettyExAnn e
    return $ "sqrt(" <> x' <> ")" <+> e'
prettyEx (Mod e x y) = 
  do
    x' <- prettyEx x
    y' <- prettyEx y
    e' <- prettyExAnn e
    return $ "mod(" <> x' <> ", " <> y' <> ")" <+> e'

prettyEx (Choose e x y) =   do
    x' <- prettyEx x
    y' <- prettyEx y
    e' <- prettyExAnn e
    return $ "choose(" <> x' <> "," <> y' <> ")" <+> e'
prettyEx (Cast e x) = 
  do
    x' <- prettyEx x
    return $ "(" <> pretty (e ^. annType) <> ")" <+> x'

prettyExBin :: Ann -> Expr -> Expr -> Doc ann -> Reader PrettyConfig (Doc ann)
prettyExBin e x y op = 
  do
    x' <- prettyEx x
    y' <- prettyEx y
    ann <- prettyExAnn e
    return $ x' <+> op <+> y' <> ann

prettyExAnn :: Ann -> Reader PrettyConfig (Doc ann)
prettyExAnn ann =
  do
    cfg <- ask
    let makeToken :: Getter PrettyConfig Bool -> Doc ann -> Doc ann
        makeToken l tok =
          if view l cfg
            then " " <> tok
            else ""
    let b = makeToken pcShowBound $
              if view annBound ann
                then "*"
                else ""
    let d = makeToken pcShowDomain $ ann ^. domain . to pretty
    let t = makeToken pcShowType $ ann ^. annType . to pretty
    let toks = [b, d, t] :: [Doc ann]
    return $ if all (T.null . show) toks then "" else " :" <> hcat toks

prettyMinimal :: Expr -> Doc ann
prettyMinimal e = runReader (prettyEx e) $ PrettyConfig 
  { _pcShowType    = False
  , _pcShowBound   = False
  , _pcShowDomain  = False
  , _pcShowAndAnns = False
  }

prettyFull :: Expr -> Doc ann
prettyFull e = runReader (prettyEx e) $ PrettyConfig 
  { _pcShowType    = True
  , _pcShowBound   = True
  , _pcShowDomain  = True
  , _pcShowAndAnns = False
  }

