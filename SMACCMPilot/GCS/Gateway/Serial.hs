{-# LANGUAGE ScopedTypeVariables #-}
module SMACCMPilot.GCS.Gateway.Serial
  ( serialServer
  ) where

import           Data.Word
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad

import qualified System.Hardware.Serialport as SP

import SMACCMPilot.GCS.Gateway.Opts
import SMACCMPilot.GCS.Gateway.Queue
import SMACCMPilot.GCS.Gateway.Monad
import SMACCMPilot.GCS.Gateway.Console
import SMACCMPilot.GCS.Gateway.Async

serialServer :: Options
             -> Console
             -> QueueOutput Word8
             -> QueueInput ByteString
             -> IO ()
serialServer opts console toser fromser =
  void $ forkIO $ SP.withSerial port settings $ server console toser fromser
  where
  settings = SP.defaultSerialSettings { SP.commSpeed = serBaud opts }
  port = serPort opts
  server consle outp inp serialport = do
    consoleLog consle "Connected to serial client"

    SP.flush serialport

    a1 <- asyncRunGW consle "serial input" $ forever $ do
      bs <- lift $ SP.recv serialport 2048
      mapM_ (queuePushGW outp) (B.unpack bs)
    a2 <- asyncRunGW consle "serial output" $ forever $
            queuePopGW inp >>= lift . (SP.send serialport)
    mapM_ wait [a1, a2]
    consoleLog consle "Disconnecting from serial client"

