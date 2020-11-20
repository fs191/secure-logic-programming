{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}

module ErrorMsg 
  ( CompilerException(..)
  , Severity(..)
  , severity
  , errorMsg
  ) where

import Relude hiding (show)

import Control.Lens

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

data CompilerException
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
  | MultipleAttributeDeclarations Expr Expr
  | UndefinedPredicate Expr
  | MultipleBindingPatterns (Text, [Bool], Expr) (Text, [Bool], Expr)
  | DBNameClash Expr Expr
  deriving (Typeable, Eq, Ord, Exception)

instance Pretty Severity where
  pretty Warning  = "[WRN]"
  pretty Error    = "[ERR]"
  pretty Internal = "[BUG]"
  pretty Debug5   = "[DB5]"
  pretty Debug4   = "[DB4]"
  pretty Debug3   = "[DB3]"
  pretty Debug2   = "[DB2]"
  pretty Debug1   = "[DB1]"

instance Show Severity where
  show = show . pretty

instance Show CompilerException where
  show = show . errorMsg ""

instance HasSeverity CompilerException where
  severity MultipleBindingPatterns{} = Warning
  severity _ = Error

instance ShowErrorComponent CompilerException where
  showErrorComponent = show

errorMsg :: Text -> CompilerException -> Doc ann
errorMsg src ex = (pretty $ severity ex) <+> (align $ errorMsg' src ex)

errorMsg' :: Text -> CompilerException -> Doc ann
errorMsg' _ NoGoal = "Program has no goal."
errorMsg' _ TooManyGoals = "Program has more than one goal."
errorMsg' _ (TypeMismatch expected got) = 
  pretty expected `expectedGot` pretty got
errorMsg' _ (ExpectedConstant expr) = 
  "a constant" `expectedGot` prettyMinimal expr
errorMsg' _ (ExpectedGroundTerm expr) = 
  "a ground term" `expectedGot` prettyMinimal expr
errorMsg' _ (NonVariableArgument expr) = 
  "a variable argument" `expectedGot` prettyMinimal expr
errorMsg' _ (CannotReadFile f ex) = vsep
  [ "Cannot read from file" <+> pretty f <> ":"
  , pretty $ show ex <> "."
  , "Ensure that the file uses a supported encoding (e.g. utf-8)."
  ]
errorMsg' _ (UnificationFailed x y) = hsep
  [ "Failed to unify expressions"
  , prettyMinimal x
  , prettyLoc x
  , "and"
  , prettyMinimal y
  , prettyLoc y
  ]
errorMsg' _ (TypeApplicationFailed t x) = vsep 
  [ hsep
    [ "Failed to apply type"
    , pretty t
    , "to expression"
    ]
  , prettyFull x
  , prettyLoc x
  ]
errorMsg' _ DoesNotConverge = "The program does not converge. Try increasing the number of iterations using `-n`"
errorMsg' _ (TypeInferenceFailed e) = "Could not infer type for\n\n" <> prettyMinimal e
errorMsg' _ (ParserException x) = "Could not parse program:\n" <> pretty x
errorMsg' s (MultipleAttributeDeclarations a b) = vsep 
  [ "Attribute @" <> prettyMinimal b <> " is already defined elsewhere at"
  , prettyPosContext (b ^. annotation . srcPos) s
  , "First defined here:"
  , prettyPosContext (a ^. annotation . srcPos) s
  ]
errorMsg' s (UndefinedPredicate p) = vsep
  [ "No rules found matching predicate " <> prettyMinimal p <> ":"
  , prettyPosContext (p ^. annotation . srcPos) s
  ]
errorMsg' s (MultipleBindingPatterns a b) = vsep
  [ "Same rule is called with different binding patterns: " 
  , prettyAdornment s a
  , "and" <+> prettyAdornment s b
  ]
errorMsg' s (DBNameClash a b) = vsep
  [ "Name clash between IDB fact"
  , prettyPosContext (a ^. annotation . srcPos) s
  , "and EDB fact"
  , prettyPosContext (b ^. annotation . srcPos) s
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
                  Just x -> pretty $ sourcePosPretty x

prettyPosContext :: Maybe SourcePos -> Text -> Doc ann
prettyPosContext pos src = srcDoc
  where
    err = "<source location invalid>"
    srcDoc = case pos of
      Just p -> vsep
        [ pretty $ sourceName p
        , filler <+> "|"
        , p'     <+> "|" <+> pretty errorLine
        , filler <+> "|" <> pretty errorIndicator
        ]
        where
          errorLine = fromMaybe err $ lines src !!? (unPos (sourceLine p) - 1)
          errorIndicator = replicate (unPos $ sourceColumn p) ' ' <> "^"
          p' = pretty . unPos $ sourceLine p
          filler = stimes (length $ show p') " "
      Nothing ->  vsep
        [ ""
        , "<missing lexeme location information>"
        , ""
        ]

prettyAdornment :: Text -> (Text, [Bool], Expr) -> Doc ann
prettyAdornment src (_, bp, pr) = vsep
  [ "with binding pattern " <> (tupled $ showBind <$> bp) <> " at"
  , prettyPosContext (pr ^. annotation . srcPos) src
  ]
  where
    showBind True  = "bound"
    showBind False = "free"

