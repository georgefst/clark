{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Control.Monad.Except
import Control.Monad.State

newtype LifxT m a = LifxT
    { unLifxT ::
        StateT
            ()
            ( ExceptT
                Error
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

data Error
    = Error
