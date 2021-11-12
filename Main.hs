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
            =<< runLifxT
                ( (\() -> void $ liftIO $ pure ())
                    =<< liftIO (pure ())
                )

runLifxT :: LifxT IO a -> IO (Maybe a)
runLifxT x = runMaybeT $ evalStateT (unLifxT x) ()
