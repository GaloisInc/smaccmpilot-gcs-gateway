{-# LANGUAGE ScopedTypeVariables #-}
module SMACCMPilot.GCS.Gateway.Serial
  ( serialServer
  ) where

import           Data.Word
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad

import           Pipes
import           Pipes.Concurrent
import           System.IO
import qualified System.Hardware.Serialport as SP

import SMACCMPilot.GCS.Gateway.Opts
import SMACCMPilot.GCS.Gateway.Console
import SMACCMPilot.GCS.Gateway.Async

serialServer :: Options -> Console -> Output Word8 -> Input ByteString  -> IO ()
serialServer opts console toser fromser = do
  sp <- SP.hOpenSerial port settings
  void $ forkIO $ server sp console toser fromser
  where
  settings = SP.defaultSerialSettings { SP.commSpeed = serBaud opts }
  port = serPort opts

server :: Handle -> Console -> Output Word8 -> Input ByteString -> IO ()
server serialport console outp inp = do
  consoleLog console "Connected to serial client"
  hSetBuffering serialport NoBuffering
  a1 <- asyncEffect "serial input" $
          serialInput serialport >-> toOutput outp
  a2 <- asyncEffect "serial output" $
          fromInput inp >-> serialOutput serialport
  finally (mapM_ wait [a1, a2])
          (hClose serialport)
  consoleLog console "Closed serial client"

serialInput :: Handle -> Producer Word8 IO r
serialInput h = forever $ do
  bs <- lift $ B.hGet h 16
  mapM_ yield (B.unpack bs)

serialOutput :: Handle -> Consumer ByteString IO r
serialOutput h = do
  b <- await
  _ <- lift $ B.hPut h b
  serialOutput h

