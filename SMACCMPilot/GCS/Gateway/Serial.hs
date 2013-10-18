{-# LANGUAGE ScopedTypeVariables #-}
module SMACCMPilot.GCS.Gateway.Serial
  ( serialServer
  ) where

import           Data.Word
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)

import           Control.Concurrent.Async
import           Control.Monad

import           Pipes
import           Pipes.Concurrent
import qualified System.Hardware.Serialport as SP

import SMACCMPilot.GCS.Gateway.Opts
import SMACCMPilot.GCS.Gateway.Console
import SMACCMPilot.GCS.Gateway.Async

serialServer :: Options -> Console -> Output Word8 -> Input ByteString  -> IO ()
serialServer opts console fromser toser = void $ forkIO $ do
  consoleLog console "Connected to serial client"
  SP.withSerial port settings $ \serialport -> do
    a1 <- asyncEffect "serial input" $
            serialInput serialport >-> toOutput fromser
    a2 <- asyncEffect "serial output" $
            fromInput toser >-> serialOutput serialport
    mapM_ wait [a1, a2]
  consoleLog console "Closed serial client"
  where
  settings = SP.defaultSerialSettings { SP.commSpeed = serBaud opts }
  port = serPort opts

serialInput :: SP.SerialPort -> Producer Word8 IO r
serialInput sp = forever $ do
  bs <- lift $ SP.recv sp 16
  mapM_ yield (B.unpack bs)

serialOutput :: SP.SerialPort -> Consumer ByteString IO r
serialOutput sp = forever $ do
  b <- await
  void $ lift $ SP.send sp b

