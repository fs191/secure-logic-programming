{-# LANGUAGE DeriveAnyClass #-}

module ErrorMsg 
  ( CompilerException(..)
  , severity
  , throw
  ) where

import Relude hiding (show)

import Control.Exception
import Control.Lens

import Data.Text.Prettyprint.Doc

import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Show

import Language.Privalog.Types

import Expr
import Annotation

class HasSeverity a where
  severity :: a -> Severity

data Severity
  = Error
  | Internal
  | Warning

data CompilerException
  = MegaparsecError (ParseErrorBundle Text Void)
  | NoGoal
  | TooManyGoals [Expr]
  | TypeMismatch PPType PPType
  | ExpectedGroundTerm Expr
  | ExpectedConstant Expr
  | NonVariableArgument Expr
  | CannotReadFile Text IOException
  | UnificationFailed Expr Expr
  | TypeApplicationFailed PPType Expr
  deriving (Typeable)

instance Exception CompilerException where
  displayException ex = show $ sev <+> pretty ex
    where sev = pretty $ severity ex

instance Pretty Severity where
  pretty Error    = "[ERROR   ]"
  pretty Internal = "[INTERNAL]"
  pretty Warning  = "[WARNING ]"

instance Show CompilerException where
  show x = show $ pretty x

instance HasSeverity CompilerException where
  severity _ = Error

instance Pretty CompilerException where
  pretty x = (pretty $ severity x) <+> (align $ errorMsg x)

errorMsg :: CompilerException -> Doc ann
errorMsg (MegaparsecError err) = pretty $ errorBundlePretty err
errorMsg NoGoal = "Program has no goal."
errorMsg (TooManyGoals x) = hsep
  [ "Program has more than one goal:"
  , indent 4 . hsep $ prettyLoc <$> x
  ]
errorMsg (TypeMismatch expected got) = 
  pretty expected `expectedGot` pretty got
errorMsg (ExpectedConstant expr) = 
  "a constant" `expectedGot` pretty expr
errorMsg (ExpectedGroundTerm expr) = 
  "a ground term" `expectedGot` pretty expr
errorMsg (NonVariableArgument expr) = 
  "a variable argument" `expectedGot` pretty expr
errorMsg (CannotReadFile f ex) = vsep
  [ "Cannot read from file" <+> pretty f <> ":"
  , pretty $ show ex <> "."
  , "Ensure that the file uses a supported encoding (e.g. utf-8)."
  ]
errorMsg (UnificationFailed x y) = hsep
  [ "Failed to unify expressions"
  , pretty x
  , prettyLoc x
  , "and"
  , pretty y
  , prettyLoc y
  ]
errorMsg (TypeApplicationFailed t x) = vsep 
  [ hsep
    [ "Failed to apply type"
    , pretty t
    , "to expression"
    ]
  , pretty x
  , prettyLoc x
  ]


-- Utilities

expectedGot :: Doc ann -> Doc ann -> Doc ann
expectedGot expected got =
  "Expected" <+> expected <+> ", but got" <+> got <+> "instead."
  

prettyLoc :: Expr -> Doc ann
prettyLoc expr = "at" <+> prettyPos
  where 
    prettyPos = case expr ^. annotation . srcPos of
                  Nothing     -> "an unknown position"
                  Just (SPos x y) -> hsep
                    [ (pretty $ sourcePosPretty x)
                    , "-"
                    , (pretty $ sourcePosPretty y)
                    ]

