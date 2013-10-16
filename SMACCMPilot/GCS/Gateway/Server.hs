{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.GCS.Gateway.Server
  ( gatewayServer
  ) where

import           Control.Concurrent.Async
import           Control.Monad
import           Data.Monoid
import           System.IO

import           Pipes
import           Pipes.Concurrent
import           Pipes.Network.TCP
import qualified Network.Simple.TCP            as N

import qualified SMACCMPilot.Communications      as Comm
import qualified SMACCMPilot.GCS.Commsec.Opts    as CS
import qualified SMACCMPilot.GCS.Gateway.Opts    as App

import           SMACCMPilot.GCS.Gateway.Console
import           SMACCMPilot.GCS.Gateway.Commsec
import           SMACCMPilot.GCS.Gateway.HXFraming
import           SMACCMPilot.GCS.Gateway.Mavlink
import           SMACCMPilot.GCS.Gateway.ByteString

gatewayServer :: CS.Options -> App.Options -> IO ()
gatewayServer csopts appopts = do

  console <- newConsole appopts stderr

  (fromveh_output, fromveh_input)     <- spawn Unbounded
  (fromradio_output, fromradio_input) <- spawn Unbounded
  (toveh_output, toveh_input)         <- spawn Unbounded

  hxframedSerial appopts (annotate console "hxserial")
                (fromveh_output)
                -- <> fromradio_output)
                toveh_input

  forkedForever $ fromInput fromradio_input
              >-> hxframeDebugger (annotate console "fromradio")
              >-> forever (await >>= const (return ()))

  commsecCtx <- mkCommsec csopts (annotate console "commsec")

  N.serve (N.Host "127.0.0.1") (show (App.srvPort appopts)) $ \(s,_) -> do
    consoleLog console "Connected to TCP client"
    forkedForever $  fromInput fromveh_input
                 >-> hxframeDebugger (annotate console "fromveh")
                 >-> hxframePayloadFromTag 0
                 >-> bytestringDebugger (annotate console "fromveh tagged")
                 >-> decrypt commsecCtx
                 >-> bytestringDebugger (annotate console "fromveh decrypted")
                 >-> toSocket s

    forever $ runEffect
            $ fromSocket s 64
          >-> bytestringDebugger (annotate console "toveh rawmavlink")
          >-> mavlinkPacketSlice (annotate console "toveh mavlinkslice")
          >-> mavlinkDebugger (annotate console "toveh mavlinkslice")
          >-> bytestringPad Comm.mavlinkSize
          >-> bytestringDebugger (annotate console "toveh plaintext")
          >-> encrypt commsecCtx
          >-> bytestringDebugger (annotate console "toveh ct")
          >-> createHXFrameWithTag 0
          >-> toOutput toveh_output

asyncEffect :: Effect IO () -> IO (Async ())
asyncEffect e = async $ runEffect e >> performGC

forkedForever :: Effect IO () -> IO ()
forkedForever e = void $ forkIO $ forever $ runEffect e

