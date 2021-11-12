import Control.Concurrent
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Lib

main :: IO ()
main = do
    m <- newEmptyMVar
    let x =
            runLifxT $
                liftIO (takeMVar m) >>= \() ->
                    sendMessage =<< pure ()
        go () = either (\_ -> go ()) pure =<< x
    go ()

sendMessage :: MonadIO f => () -> f ()
sendMessage () = void . liftIO $ undefined

runLifxT :: Monad m => LifxT m a -> m (Either Error a)
runLifxT x = runExceptT $ evalStateT (unLifxT x) ()
