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
            =<< runTrans
                ( (\() -> void $ liftIO $ pure ())
                    =<< liftIO (pure ())
                )

runTrans :: Trans IO a -> IO (Maybe a)
runTrans (Trans x) = runMaybeT $ evalStateT x ()
