{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Logger 
  ( LoggerT
  , log
  , execLogT
  ) where

import Relude

import Control.Monad.Writer

import ErrorMsg

newtype LoggerT m a = LoggerT
      (ReaderT Int
        (WriterT Text m) a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans LoggerT where
  lift = LoggerT . lift . lift

class MonadLogger m where
  log :: Severity -> Text -> m ()

instance (Monad m) => MonadLogger (LoggerT m) where
  log s msg = LoggerT $
    do
      level <- ask
      let message = show s <> " " <> show msg
      when (level <= fromEnum s) $ tell message

execLogT
  :: (Monad m)
  => Int 
  -> LoggerT m a 
  -> m Text
execLogT level (LoggerT x) = execWriterT $ runReaderT x level

