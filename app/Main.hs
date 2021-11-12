{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Text ()
import Data.Text.Encoding
import Data.Time
import Data.Word
import Lifx.Lan hiding (SetColor)
import Network.Socket
import Network.Socket.ByteString
import Options.Generic
import System.Console.ANSI
import System.IO
import System.Process.Extra
import Text.Pretty.Simple hiding (Blue, Color, Green, Red, Vivid)

--TODO rename fields when we have OverloadedRecordDot (GHC 9.2), and thus simplify the `ParseRecord` instance
data Opts = Opts
    { optButtonDebounce :: Double
    , optButtonPin :: Int
    , optLightName :: Text
    , optLifxTimeout :: Double
    , optReceivePort :: Word16
    }
    deriving (Show, Generic)
instance ParseRecord Opts where
    parseRecord =
        parseRecordWithModifiers
            defaultModifiers
                { fieldNameModifier = fieldNameModifier lispCaseModifiers . drop (length @[] "opt")
                }

data Action
    = ToggleLight

main :: IO ()
main = do
    Opts{..} <- getRecord "Clark"
    let light = deviceFromAddress (192, 168, 1, 190)

    let listenOnNetwork = do
            sock <- socket AF_INET Datagram defaultProtocol
            bind sock $ SockAddrInet (fromIntegral optReceivePort) 0
            forever $ runLifxUntilSuccess (lifxTime optLifxTimeout) do
                bs <- liftIO $ recv sock 1
                liftIO $ withSGR' Blue $ BS.putStrLn $ "Received UDP message: " <> bs
                toggleLight light

    let listenForButton = do
            putStrLn "Starting gpiomon process..."
            hs@(_, Just gpiomonStdout, _, _) <-
                createProcess
                    (proc "gpiomon" ["-b", "-f", "gpiochip0", show optButtonPin])
                        { std_out = CreatePipe
                        }
            putStrLn "Done!"

            handle (\(e :: IOException) -> print e >> cleanupProcess hs) . runLifxUntilSuccess (lifxTime optLifxTimeout) $
                liftIO getCurrentTime >>= iterateM_ \t0 -> do
                    line <- liftIO $ hGetLine gpiomonStdout
                    t1 <- liftIO getCurrentTime
                    if diffUTCTime t1 t0 < realToFrac optButtonDebounce
                        then liftIO $ withSGR' Red $ putStr "Ignoring: "
                        else do
                            liftIO $ withSGR' Green $ putStr "Ok: "
                            toggleLight light
                    liftIO $ putStrLn line
                    pure t1

    listenOnNetwork `concurrently_` listenForButton

toggleLight :: MonadLifx m => Device -> m ()
toggleLight light = sendMessage light . SetPower . not . statePowerToBool =<< sendMessage light GetPower

--TODO upstream?
statePowerToBool :: StatePower -> Bool
statePowerToBool = (/= StatePower 0)

--TODO lifx-lan should probably use a time library type, rather than Int
lifxTime :: Double -> Int
lifxTime = round . (* 1_000_000)

--TODO upstream? not the first time I've defined this
withSGR :: [SGR] -> IO a -> IO a
withSGR sgr x = do
    setSGR sgr
    r <- x
    setSGR [Reset]
    pure r

-- A special case of 'withSGR'
withSGR' :: MonadIO m => Color -> IO a -> m a
withSGR' x = liftIO . withSGR [SetColor Foreground Vivid x, SetConsoleIntensity BoldIntensity]

-- | Run the action. If it fails then just print the error and go again.
runLifxUntilSuccess :: Int -> Lifx a -> IO a
runLifxUntilSuccess t x = either (\e -> print e >> runLifxUntilSuccess t x) pure =<< runLifxT t x
