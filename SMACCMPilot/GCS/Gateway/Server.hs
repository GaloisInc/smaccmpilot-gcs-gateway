{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.GCS.Gateway.Server
  ( gatewayServer
  ) where

import           Control.Monad
import           Data.Monoid
import           System.IO

import           Pipes
import           Pipes.Concurrent
import           Pipes.Network.TCP
import qualified Network.Simple.TCP              as N

import qualified SMACCMPilot.Communications      as Comm
import qualified SMACCMPilot.GCS.Commsec.Opts    as CS
import qualified SMACCMPilot.GCS.Gateway.Opts    as App

import           SMACCMPilot.GCS.Gateway.Console
import           SMACCMPilot.GCS.Gateway.Commsec
import           SMACCMPilot.GCS.Gateway.HXFraming
import           SMACCMPilot.GCS.Gateway.Mavlink
import           SMACCMPilot.GCS.Gateway.ByteString
import           SMACCMPilot.GCS.Gateway.Link

gatewayServer :: CS.Options -> App.Options -> IO ()
gatewayServer csopts appopts = do

  console <- newConsole appopts stderr

  -- We want to broadcast messages from the radio onto two
  -- different channels, one to the link managment, one to the
  -- commsec server:
  (fromradio_output,  fromradio_input)  <- spawn Unbounded
  (fromradio_output', fromradio_input') <- spawn Unbounded
  -- Both the link managment and commsec server will write to
  -- this channel:
  (toradio_output,    toradio_input)    <- spawn Unbounded

  hxframedSerial appopts (annotate console "hxserial")
                (fromradio_output <> fromradio_output')
                toradio_input

  linkManagment (annotate console "link") toradio_output fromradio_input

  commsecCtx <- mkCommsec csopts (annotate console "commsec")

  N.serve (N.Host "127.0.0.1") (show (App.srvPort appopts)) $ \(s,_) -> do
    consoleLog console "Connected to TCP client"
    forkEffect $  fromInput fromradio_input'
--                 >-> hxframeDebugger (annotate console "fromveh raw")
                 >-> hxframePayloadFromTag 0
--                 >-> bytestringDebugger (annotate console "fromveh tagged")
                 >-> decrypt commsecCtx
--                 >-> bytestringDebugger (annotate console "fromveh decrypted")
                 >-> mavlinkPacketSlice (annotate console "fromveh mavlinkslice")
--                 >-> mavlinkDebugger (annotate console "fromveh")
                 >-> toSocket s

    runEffect $ fromSocket s 64
--          >-> bytestringDebugger (annotate console "toveh raw")
          >-> mavlinkPacketSlice (annotate console "toveh mavlinkslice")
          >-> mavlinkDebugger (annotate console "toveh")
          >-> bytestringPad Comm.mavlinkSize
--          >-> bytestringDebugger (annotate console "toveh plaintext")
          >-> encrypt commsecCtx
--          >-> bytestringDebugger (annotate console "toveh ct")
          >-> createHXFrameWithTag 0
          >-> toOutput toradio_output

forkEffect :: Effect IO () -> IO ()
forkEffect e = void $ forkIO $ runEffect e

