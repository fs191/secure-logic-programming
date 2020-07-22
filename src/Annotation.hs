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
  , typing
  ) where

import Control.Lens

import Data.Data
import Data.Foldable

import Language.SecreC.Types

data Typing
  = Typing PPDomain PPType
  deriving (Show)

data Ann = Ann
  { 
    -- Datatype of the term
    _annType  :: PPType
    -- Security domain of the term
  , _domain   :: PPDomain
  , _annBound :: Bool
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

empty :: Ann
empty = Ann PPAuto Unknown False

emptyTyping :: Typing
emptyTyping = Typing Unknown PPAuto

-- | Unify types and domains. Return Nothing if the types do not unify.
unifyAnns :: Ann -> Ann -> Ann
unifyAnns x y = x & typing %~ unifyTypings (y ^. typing)

typing :: Lens' Ann Typing
typing = lens getter setter
  where 
    getter ann = Typing (ann ^. domain) (ann ^. annType)
    setter ann (Typing d t) = ann & domain  .~ d
                                  & annType .~ t

-- | Unifies two typings
unifyTypings :: Typing -> Typing -> Typing
unifyTypings (Typing xd xt) (Typing yd yt)
  =  Typing (unifyDomains xd yd) (unifyTypes xt yt)
