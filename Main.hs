{-# OPTIONS_GHC -Wall #-}

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Lib

main :: IO ()
main = go ()
  where
    go () =
        maybe (go ()) pure
            =<< runM
                ( (\() -> void $ liftIO $ pure ())
                    =<< liftIO (pure ())
                )

runM :: Trans IO a -> IO (Maybe a)
runM x = runMaybeT $ evalStateT (unTrans x) ()
