{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.GCS.Gateway.Server
  ( gatewayServer
  ) where

import           Control.Concurrent.Async
import           Data.Monoid
import           System.IO

import           Pipes
import           Pipes.Concurrent
import           Pipes.Network.TCP
import qualified Network.Simple.TCP            as N

import qualified SMACCMPilot.GCS.Commsec.Opts    as CS
import qualified SMACCMPilot.GCS.Gateway.Opts    as App

-- import qualified SMACCMPilot.Communications      as Comm
-- import qualified SMACCMPilot.Mavlink.Parser      as ML
-- import qualified SMACCMPilot.Mavlink.MessageName as ML

import           SMACCMPilot.GCS.Gateway.Console
import           SMACCMPilot.GCS.Gateway.Commsec
import           SMACCMPilot.GCS.Gateway.HXFraming

gatewayServer :: CS.Options -> App.Options -> IO ()
gatewayServer csopts appopts = do

  console <- newConsole appopts stderr

  (fromveh_output, fromveh_input)     <- spawn Unbounded
  (fromradio_output, fromradio_input) <- spawn Unbounded
  (toveh_output, toveh_input)         <- spawn Unbounded

  hxframedSerial appopts console (fromveh_output <> fromradio_output) toveh_input

  a <- asyncEffect $ fromInput fromradio_input
                 >-> hxframeDebugger console "fromveh"

  N.serve (N.Host "127.0.0.1") (show (App.srvPort appopts)) $ \(s,_) -> do
    consoleLog console "Connected to TCP client"
    a2 <- asyncEffect $ fromInput fromveh_input
                    >-> hxframePayloadFromTag 0
                    >-> decryptionPipeline csopts console
                    >-> toSocket s

    a1 <- asyncEffect $ fromSocket s 16
                    >-> encryptionPipeline csopts console
                    >-> createHXFrameWithTag 0
                    >-> toOutput toveh_output

    mapM_ wait [a, a1, a2]
  consoleLog console "Exiting"

asyncEffect :: Effect IO () -> IO (Async ())
asyncEffect e = async $ runEffect e >> performGC

