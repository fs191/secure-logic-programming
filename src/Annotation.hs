{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Annotation 
  ( Ann(..)
  , Typing(..)
  , empty, emptyTyping
  , annType, domain
  , annBound
  , unifyAnns
  , unifyTypings
  , safelyUnifyDomains, safelyUnifyTypings
  , typing
  , srcPos
  ) where

import Control.Lens

import Data.Data
import Data.Foldable

import Language.SecreC.Types

import Text.Megaparsec.Pos

-- | A data type used for unifying type and domain simultaneously
data Typing
  = Typing PPDomain PPType
  deriving (Show)

-- | Annotations data type for storing extra information about expressions
data Ann = Ann
  { 
    -- | Datatype of the term
    _annType  :: PPType
    -- | Security domain of the term.
    -- It tells whether a value is public, private or has an unknown domain.
  , _domain   :: PPDomain
    -- | Tells wether the expression is bound
  , _annBound :: Bool
  , _srcPos   :: Maybe (SourcePos, SourcePos)
  }
  deriving (Ord, Eq, Data, Typeable)
makeLenses ''Ann

instance Show Ann where
  show x = ": " <> s
    where s = asum 
            [ if x ^. annBound then "*" else ""
            , x ^. domain . to show
            , " "
            , x ^. annType . to show
            ]

-- | Returns the default annotation that is untyped and unbound.
empty :: Ann
empty = Ann PPAuto Unknown True Nothing

-- | Returns an empty typing with no domain and auto type
emptyTyping :: Typing
emptyTyping = Typing Unknown PPAuto

-- | Unify types and domains. Return Nothing if the types do not unify.
unifyAnns :: Ann -> Ann -> Ann
unifyAnns x y = x & typing %~ unifyTypings (y ^. typing)

-- | Lens for accessing the typing of an annotation
typing :: Lens' Ann Typing
typing = lens getter setter
  where 
    getter ann = Typing (ann ^. domain) (ann ^. annType)
    setter ann (Typing d t) = ann & domain  .~ d
                                  & annType .~ t

-- | Unifies two typings so that unknown domain gets overwritten by anything
-- else. Useful for assigning a typing to an expression that might already have
-- a typing.
unifyTypings :: Typing -> Typing -> Typing
unifyTypings (Typing xd xt) (Typing yd yt)
  =  Typing (unifyDomains xd yd) (unifyTypes xt yt)

-- | Unifies domains in a conservative way, so that if one of the terms is
-- unknown and the other public, then the result will be unknown.
safelyUnifyDomains :: PPDomain -> PPDomain -> PPDomain
safelyUnifyDomains Private _ = Private
safelyUnifyDomains _ Private = Private
safelyUnifyDomains Unknown _ = Unknown
safelyUnifyDomains _ Unknown = Unknown
safelyUnifyDomains _ _       = Public

safelyUnifyTypings :: Typing -> Typing -> Typing 
safelyUnifyTypings (Typing dx tx) (Typing dy ty)
  = Typing (safelyUnifyDomains dx dy) $ unifyTypes tx ty

