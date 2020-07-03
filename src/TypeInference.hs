module TypeInference
  ( typeInference
  ) where

import Control.Lens

import Data.Map

import Language.SecreC.Types
import DatalogProgram

data VarInfo = VarInfo
  { _viDom  :: PPDomain
  , _viType :: PPType
  }

data InfrenceState = InfrenceState
  { _isScopeInfo :: Map String VarInfo
  }

typeInference :: DatalogProgram -> DatalogProgram
typeInference dp = undefined
  where
    _goal       = dp ^. dpGoal
    _inputTypes = inputs dp

