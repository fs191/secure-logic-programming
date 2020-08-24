{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module ErrorMsg 
  ( ParserError(..)
  , TypeCheckerError(..)
  , SemanticsError(..)
  , TransformError(..)
  , CodeGenerationError(..)
  , CSVImportError(..)
  ) where

import Control.Exception
import Control.Lens

import Data.Void
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

import Text.Megaparsec.Error
import Text.Megaparsec.Pos

import Language.SecreC.Types

import Expr
import Annotation

class HasSeverity a where
  severity :: a -> Severity

data ErrorDetails = ErrorDetails
  {
  }

data CompilerException
  = CompilerException ErrorDetails CompilerError

data Severity
  = Error
  | Internal
  | Warning

data CompilerError
  = CEParserError ParserError
  | CETypeCheckerError TypeCheckerError
  | CETransformError TransformError
  | CECodeGenerationError CodeGenerationError
  | CECSVImportError CSVImportError
  | CESemanticsError SemanticsError

data ParserError
  = MegaparsecError (ParseErrorBundle Text Void)
  | NoGoal
  | TooManyGoals [Expr]
  deriving (Exception)

data TypeCheckerError 
  = TypeMismatch PPType PPType
  deriving (Exception)

data SemanticsError
  = ExpectedGroundTerm Expr
  | ExpectedConstant Expr
  | NonVariableArgument Expr
  deriving (Exception)

data TransformError
  = TransformError
  deriving (Exception)

data CodeGenerationError
  = CodeGenerationError
  deriving (Exception)

data CSVImportError
  = CSVImportError
  deriving (Exception)

instance Pretty CompilerException where
  pretty (CompilerException _ err) = pretty (severity err) <+> pretty err

instance Show CompilerException where
  show = show . pretty

instance HasSeverity CompilerException where
  severity (CompilerException _ err) = severity err

instance Pretty Severity where
  pretty Error    = "[ERROR   ]"
  pretty Internal = "[INTERNAL]"
  pretty Warning  = "[WARNING ]"

instance Pretty CompilerError where
  pretty (CEParserError err) = pretty err
  pretty (CETypeCheckerError err) = pretty err
  pretty (CETransformError err) = pretty err
  pretty (CECodeGenerationError err) = pretty err
  pretty (CECSVImportError err) = pretty err
  pretty (CESemanticsError err) = pretty err

instance Show CompilerError where
  show = show . pretty

instance HasSeverity CompilerError

instance Pretty ParserError where
  pretty (MegaparsecError err) = pretty $ errorBundlePretty err
  pretty NoGoal = "Program has no goal."
  pretty (TooManyGoals x) = hsep
    [ "Program has more than one goal:"
    , indent 4 . hsep $ prettyLoc <$> x
    ]

instance Show ParserError where
  show = show . pretty

instance HasSeverity ParserError where
  severity (MegaparsecError _) = Error

instance Pretty TypeCheckerError where
  pretty (TypeMismatch expected got) = 
    pretty expected `expectedGot` pretty got

instance Show TypeCheckerError where
  show = show . pretty

instance HasSeverity TypeCheckerError where
  severity (TypeMismatch _ _) = Error

instance Pretty TransformError where
  pretty _ = undefined

instance Show TransformError where
  show = show . pretty

instance Pretty CodeGenerationError where
  pretty _ = undefined

instance Show CodeGenerationError where
  show = show . pretty

instance Pretty CSVImportError where
  pretty _ = undefined

instance Show CSVImportError where
  show = show . pretty

instance Pretty SemanticsError where
  pretty (ExpectedConstant expr) = hsep
    [ "Expected a constant, but got"
    , pretty expr
    , "instead"
    ]
  pretty (ExpectedGroundTerm expr) = 
    "a ground term" `expectedGot` pretty expr
  pretty (NonVariableArgument expr) = 
    "a variable argument" `expectedGot` pretty expr

instance Show SemanticsError where
  show = show . pretty

-- Utilities

expectedGot :: Doc ann -> Doc ann -> Doc ann
expectedGot expected got = hsep
  [ "Expected"
  , expected
  , ", but got"
  , got
  , "instead."
  ]

prettyLoc :: Expr -> Doc ann
prettyLoc expr = "at" <+> prettyPos
  where 
    prettyPos = case expr ^. annotation . srcPos of
                  Nothing     -> "an unknown position"
                  Just (x, y) -> hsep
                    [ (pretty $ sourcePosPretty x)
                    , "-"
                    , (pretty $ sourcePosPretty y)
                    ]

-- the term t contains the error message generated inside megaparsec

-- csv import errors
--error_unknownColumn x y        = errorTag ++ "Could not find column " ++ show x ++ " of table " ++ show y ++ "."
--error_unsupportedColumnType x  = errorTag ++ "Column type " ++ show x ++ " is not supported."
--
--error_dbFileLengthsTooMany x ys = errorTag ++ "Excessive row records " ++ show ys ++ " in the table " ++ show x ++ "."
--error_dbFileLengthsTooFew x ys = errorTag ++ "Insufficient row records for attributes " ++ show ys ++ " in the table " ++ show x ++ "."

---- transformation errors
---- TODO we could probably track the error location better...
--error_nonGroundTableVar t x i  = inErrTag ++ "Table " ++ show t ++ " is called with a free variable " ++ show x ++ " on argument " ++ show i ++ " after unfolding."
--
---- secrec code generation errors
--error_complexExpression x  = errorTag ++ "Could not process " ++ show x ++ ", only variables are supported as predicate arguments."
--error_tableArgNotFound t x i = inErrTag ++ show i ++ "-th arg of " ++ show t ++ ", " ++ show x ++ " has not been included into cross product table."
--error_argNotFound x          = inErrTag ++ " var " ++ show x ++ " has no matching in the argument list."

