{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.GCS.Gateway.Server
  ( gatewayServer
  ) where

import qualified Data.ByteString                 as B
import qualified Control.Concurrent              as C
import qualified Control.Concurrent.Async        as A
import           Control.Monad
import           System.IO

import qualified Network.Simple.TCP              as N
import qualified SMACCMPilot.Communications as Comm

import qualified SMACCMPilot.GCS.Commsec.Opts    as CS
import qualified SMACCMPilot.GCS.Gateway.Opts    as App

import           SMACCMPilot.GCS.Gateway.Console
import           SMACCMPilot.GCS.Gateway.Commsec
import           SMACCMPilot.GCS.Gateway.HXFraming
import           SMACCMPilot.GCS.Gateway.Mavlink
import           SMACCMPilot.GCS.Gateway.ByteString
import           SMACCMPilot.GCS.Gateway.Link
import           SMACCMPilot.GCS.Gateway.Async
import           SMACCMPilot.GCS.Gateway.Queue
import           SMACCMPilot.GCS.Gateway.Monad
import           SMACCMPilot.GCS.Gateway.Packing

gatewayServer :: CS.Options -> App.Options -> IO ()
gatewayServer csopts appopts = do

  console <- newConsole appopts stderr

  -- We want to broadcast messages from the radio onto two
  -- different channels, one to the link managment, one to the
  -- commsec server:
  (fromradio_output,  fromradio_input)  <- newQueue
  (fromradio_output', fromradio_input') <- newQueue
  -- Both the link managment and commsec server will write to
  -- this channel:
  (toradio_output,    toradio_input)    <- newQueue

  -- Both the link managment and radio-to-socket pipeline
  -- the output of this channel:
  (tosocket_output,    tosocket_input)    <- newQueue

  hxframedSerial appopts (annotate console "hxserial")
                [fromradio_output, fromradio_output']
                toradio_input

  linkManagment console toradio_output fromradio_input

  commsecCtx <- mkCommsec csopts

  case App.testMode appopts of
    Nothing -> return ()
    Just n -> do
        let rate = read n :: Int
        putStrLn ("Test mode running at " ++ show rate ++ "hz")
        runGW console $ forever $
          lift (C.threadDelay (1000000 `div` rate)) >>
          bytestringPad Comm.mavlinkSize (B.pack [2,3,4,5]) >>=
            (createHXFrameWithTag 0 >=> queuePushGW toradio_output)

  N.serve (N.Host "127.0.0.1") (show (App.srvPort appopts)) $ \(s,_) -> do
    consoleLog console "Connected to TCP client"
    pktslicer1 <- mkMavlinkPacketSlice
    pktslicer2 <- mkMavlinkPacketSlice
    packer <- mkPacker Comm.mavlinkSize
    a1 <- asyncRunGW console "radio to socket queue" $ forever $
            queuePopGW fromradio_input' >>=
              fromradio tosocket_output pktslicer1 commsecCtx
    a2 <- asyncRunGW console "socket queue to socket" $ forever $
            queuePopGW tosocket_input >>= lift . (N.send s)
    a3 <- asyncRunGW console "socket to radio" $ forever $
            lift (N.recv s 2048) >>~
              fromsock toradio_output pktslicer2 packer commsecCtx

    mapM_ A.wait [a1, a2, a3]
    where
    fromradio q mavlinkPacketSlice commsecCtx =
      hxframeDebugger "fromveh raw"           >=>
      hxframePayloadFromTag 0                 >~>
--      bytestringDebugger "fromveh tagged"     >=>
      decrypt commsecCtx                      >~>
--      bytestringDebugger "fromveh decrypted"  >=>
      mavlinkPacketSlice                      >~>
      mavlinkDebugger "fromveh"               >=>
      queuePushGW q

    fromsock q mavlinkPacketSlice packer commsecCtx =
--      bytestringDebugger "toveh raw"       >=>
      mavlinkPacketSlice                   >~>
      mavlinkDebugger "toveh"              >=>
      packer                               >~>
      bytestringPad Comm.mavlinkSize       >=>
      bytestringDebugger "toveh plaintext" >=>
      encrypt commsecCtx                   >~>
--      bytestringDebugger "toveh ct"        >=>
      createHXFrameWithTag 0               >=>
      queuePushGW q



