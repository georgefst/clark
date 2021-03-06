module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Text ()
import Data.Time
import Data.Word
import Lifx.Lan hiding (SetColor)
import Network.Socket
import Network.Socket.ByteString
import Options.Generic
import System.Console.ANSI
import System.IO
import System.Process.Extra
import Text.Pretty.Simple (pPrint)

data Opts = Opts
    { buttonDebounce :: Double
    , buttonPin :: Int
    , ledPins :: [Int]
    , lightName :: Text
    , lifxTimeout :: Double
    , receivePort :: Word16
    }
    deriving (Show, Generic)
instance ParseRecord Opts where
    parseRecord =
        parseRecordWithModifiers
            defaultModifiers
                { fieldNameModifier = fieldNameModifier lispCaseModifiers
                }

data Action
    = ToggleLight
    deriving (Show)

main :: IO ()
main = do
    (opts :: Opts) <- getRecord "Clark"
    callProcess "gpioset" $ "gpiochip0" : map ((<> "=0") . show) opts.ledPins -- ensure all LEDs are off to begin with
    mvar <- newEmptyMVar
    --TODO avoid hardcoding - discovery doesn't currently work on Clark (firewall?)
    let light = deviceFromAddress (192, 168, 1, 190)

    let listenOnNetwork = do
            sock <- socket AF_INET Datagram defaultProtocol
            bind sock $ SockAddrInet (fromIntegral opts.receivePort) 0
            forever do
                bs <- recv sock 1
                withSGR' Blue $ BSC.putStrLn $ "Received UDP message: " <> bs
                let action = decodeAction bs
                pPrint action --TODO better logging
                maybe mempty (putMVar mvar) $ decodeAction bs

    let listenForButton = do
            putStrLn "Starting gpiomon process..."
            hs@(_, Just gpiomonStdout, _, _) <-
                createProcess
                    (proc "gpiomon" ["-b", "-f", "gpiochip0", show opts.buttonPin])
                        { std_out = CreatePipe
                        }
            putStrLn "Done!"

            handle (\(e :: IOException) -> print e >> cleanupProcess hs) $
                getCurrentTime >>= iterateM_ \t0 -> do
                    line <- hGetLine gpiomonStdout
                    t1 <- getCurrentTime
                    if diffUTCTime t1 t0 < realToFrac opts.buttonDebounce
                        then withSGR' Red $ putStr "Ignoring: "
                        else do
                            withSGR' Green $ putStr "Ok: "
                            putMVar mvar ToggleLight
                    putStrLn line
                    pure t1

    listenOnNetwork `concurrently_` listenForButton `concurrently_` runLifxUntilSuccess (lifxTime opts.lifxTimeout) do
        forever $
            liftIO (takeMVar mvar) >>= \case
                ToggleLight -> toggleLight light

decodeAction :: ByteString -> Maybe Action
decodeAction bs = case BS.unpack bs of
    [1] -> Just ToggleLight
    _ -> Nothing

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
runLifxUntilSuccess t x = either (\e -> print e >> threadDelay t >> runLifxUntilSuccess t x) pure =<< runLifxT t x
