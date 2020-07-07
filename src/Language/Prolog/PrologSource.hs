module Language.Prolog.PrologSource 
  ( PrologSource(..)
  ) where

import Data.Text.Prettyprint.Doc

class PrologSource a where
  -- | Pretty-print data as valid prolog source code
  prolog :: a -> Doc ann

