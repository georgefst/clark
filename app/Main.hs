module Main (main) where

import Control.Concurrent
import Control.Monad.IO.Class
import Lifx.Lan

main :: IO ()
main = do
    m <- newEmptyMVar
    let x =
            runLifxT 0 $
                liftIO (takeMVar m) >>= \() ->
                    sendMessage (deviceFromAddress (192, 168, 1, 190)) =<< pure (SetPower True)
        go () = either (\_ -> go ()) pure =<< x
    go ()
