{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module Lib where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Maybe

newtype LifxT m a = LifxT
    { unLifxT ::
        StateT
            ()
            ( MaybeT
                m
            )
            a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        )
