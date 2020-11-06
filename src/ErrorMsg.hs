{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}

module ErrorMsg 
  ( CompilerException(..)
  , CompilerExceptionInfo(..)
  , Severity(..)
  , severity
  , errorMsgWithSource
  , compEx_, compEx, throwCompEx
  ) where

import Relude hiding (show)

import Control.Lens
import Control.Monad.Except

import Data.Text.Prettyprint.Doc

import Text.Megaparsec.Pos
import Text.Show

import Language.Privalog.Types

import Expr
import ExprPretty
import Annotation
import Text.Megaparsec

class HasSeverity a where
  severity :: a -> Severity

data Severity
  = Warning
  | Error
  | Internal
  | Debug1 -- Least detailed
  | Debug2
  | Debug3
  | Debug4
  | Debug5 -- Most detailed
  deriving (Enum, Eq, Ord)

data CompilerException = 
  CompilerException CompilerExceptionInfo (Maybe SourcePos)
  deriving (Typeable, Eq, Ord)

data CompilerExceptionInfo
  = NoGoal
  | ParserException Text
  | DoesNotConverge
  | TypeInferenceFailed Expr
  | TooManyGoals
  | TypeMismatch PPType PPType
  | ExpectedGroundTerm Expr
  | ExpectedConstant Expr
  | NonVariableArgument Expr
  | CannotReadFile Text Text
  | UnificationFailed Expr Expr
  | TypeApplicationFailed PPType Expr
  deriving (Typeable, Eq, Ord)

instance Exception CompilerException where
  displayException ex = show $ sev <+> (align $ pretty ex)
    where sev = pretty $ severity ex

instance ShowErrorComponent CompilerException where
  showErrorComponent = displayException

instance Pretty Severity where
  pretty Warning  = "[WARNING ]"
  pretty Error    = "[ERROR   ]"
  pretty Internal = "[INTERNAL]"
  pretty Debug5   = "[DEBUG 5 ]"
  pretty Debug4   = "[DEBUG 4 ]"
  pretty Debug3   = "[DEBUG 3 ]"
  pretty Debug2   = "[DEBUG 2 ]"
  pretty Debug1   = "[DEBUG 1 ]"

instance Show Severity where
  show = show . pretty

instance Show CompilerException where
  show = show . pretty

instance Show CompilerExceptionInfo where
  show = show . errorMsg

instance HasSeverity CompilerExceptionInfo where
  severity _ = Error

instance HasSeverity CompilerException where
  severity (CompilerException info _) = severity info

instance Pretty CompilerException where
  pretty (CompilerException info _) = 
    (pretty $ severity info) <+> (align $ errorMsg info)

errorMsgWithSource :: Text -> CompilerException -> Doc ann
errorMsgWithSource src (CompilerException info pos) = vsep [errorMsg info, srcDoc]
  where
    err = "<source location invalid>"
    srcDoc = case pos of
      Just p -> vsep
        [ pretty $ sourceName p
        , " |"
        , " |" <+> pretty errorLine
        , " |" <+> pretty errorIndicator
        ]
        where
          errorLine = fromMaybe err $ lines src !!? (unPos $ sourceLine p)
          errorIndicator = replicate (unPos $ sourceColumn p) ' ' <> "^"
      Nothing -> emptyDoc

errorMsg :: CompilerExceptionInfo -> Doc ann
errorMsg NoGoal = "Program has no goal."
errorMsg TooManyGoals = "Program has more than one goal."
errorMsg (TypeMismatch expected got) = 
  pretty expected `expectedGot` pretty got
errorMsg (ExpectedConstant expr) = 
  "a constant" `expectedGot` prettyMinimal expr
errorMsg (ExpectedGroundTerm expr) = 
  "a ground term" `expectedGot` prettyMinimal expr
errorMsg (NonVariableArgument expr) = 
  "a variable argument" `expectedGot` prettyMinimal expr
errorMsg (CannotReadFile f ex) = vsep
  [ "Cannot read from file" <+> pretty f <> ":"
  , pretty $ show ex <> "."
  , "Ensure that the file uses a supported encoding (e.g. utf-8)."
  ]
errorMsg (UnificationFailed x y) = hsep
  [ "Failed to unify expressions"
  , prettyMinimal x
  , prettyLoc x
  , "and"
  , prettyMinimal y
  , prettyLoc y
  ]
errorMsg (TypeApplicationFailed t x) = vsep 
  [ hsep
    [ "Failed to apply type"
    , pretty t
    , "to expression"
    ]
  , prettyFull x
  , prettyLoc x
  ]
errorMsg DoesNotConverge = "The program does not converge. Try increasing the number of iterations using `-n`"
errorMsg (TypeInferenceFailed e) = "Could not infer type for\n\n" <> prettyMinimal e
errorMsg (ParserException x) = "Could not parse program:\n" <> pretty x

-- Utilities

expectedGot :: Doc ann -> Doc ann -> Doc ann
expectedGot expected got =
  "Expected" <+> expected <+> ", but got" <+> got <+> "instead."
  

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

throwCompEx
  :: (MonadError CompilerException m) 
  => CompilerExceptionInfo 
  -> SourcePos 
  -> m ()
throwCompEx info pos = throwError $ compEx info pos

compEx :: CompilerExceptionInfo -> SourcePos -> CompilerException
compEx info pos = CompilerException info $ Just pos

compEx_ :: CompilerExceptionInfo -> CompilerException
compEx_ x = CompilerException x Nothing

