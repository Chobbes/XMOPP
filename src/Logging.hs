{-# LANGUAGE TypeFamilies #-}
module Logging where

import Control.Monad.Trans
import Control.Monad.Logger
import Control.Monad.Primitive

instance PrimMonad m => PrimMonad (LoggingT m) where
  type PrimState (LoggingT m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance PrimMonad m => PrimMonad (NoLoggingT m) where
  type PrimState (NoLoggingT m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}
