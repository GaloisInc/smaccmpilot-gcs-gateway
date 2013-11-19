
module SMACCMPilot.GCS.Gateway.Link
  ( linkManagment
  ) where

import           Control.Monad
import qualified Control.Concurrent              as C
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as L
import           Data.ByteString (ByteString)
import           Data.Char (ord)
import           Data.IORef
import           Data.Word

import           SMACCMPilot.GCS.Gateway.Monad
import           SMACCMPilot.GCS.Gateway.Async
import           SMACCMPilot.GCS.Gateway.Queue
import           SMACCMPilot.GCS.Gateway.Console
import           SMACCMPilot.GCS.Gateway.HXFraming
import           SMACCMPilot.GCS.Gateway.ByteString
import qualified SMACCMPilot.GCS.Gateway.RadioDebug  as D
import qualified SMACCMPilot.GCS.Gateway.RadioStatus as S
import           SMACCMPilot.GCS.Gateway.Mavlink

import           SMACCMPilot.GCS.Mavlink.Send
import           SMACCMPilot.GCS.Mavlink.Messages.GcsRadio



linkManagment :: Console
              -> QueueOutput HXFrame
              -> QueueInput HXFrame
              -> QueueOutput ByteString
              -> IO ()
linkManagment console link_output link_input mavlink_output = do
  void $ asyncRunGW console "link process" $ do
    mavlinkstate <- lift $ newIORef defaultMavlinkState
    forever $ queuePopGW link_input   >>=
             (hxframePayloadFromTag 1 >~>
              process mavlinkstate)

  void $ asyncRunGW console "link request" $ forever $ do
    lift $ C.threadDelay 1000000
    createHXFrameWithTag 1 statReqFrame >>= queuePushGW link_output
  where
  defaultMavlinkState = MavlinkState { sysid = 77, compid = 0, seqnum = 0 }
  statReqFrame = B.pack [0x42, 0x0d] -- "B\r"
  dbgReqFrame  = B.pack [0x44, 0x0d] -- "D\r"

  process :: IORef MavlinkState -> ByteString -> GW ()
  process mavlinkstate frame =
    case tag of
      0x42 -> processRadioStat
      0x44 -> processRadioDebug
      _    -> processRadioOther
    where
    tag = head (B.unpack frame)

    processRadioDebug =
      case D.parseRadioDbg frame of
        Just s -> D.debugRadioDbg s
        Nothing -> writeDbg "Bad radio debug packet"

    processRadioOther =
      bytestringDebugger "Unknown packet" frame >> return ()

    processRadioStat =
      case S.parseRadioStatus frame of
        Nothing -> writeDbg "Bad radio stat packet"
        Just s -> do
          S.debugRadioStatus s
          state <- lift $ readIORef mavlinkstate
          let payload = mkGcsRadioMessage s
          case mavlinkSend state gcsRadioMsgId payload of
            Just (msg, state') -> do
              lift $ writeIORef mavlinkstate state'
              _ <- mavlinkDebugger "processRadioStat" (L.toStrict msg)
              queuePushGW mavlink_output (L.toStrict msg)
            Nothing -> writeDbg
              "Malformed mavlink packet in Gateway.Link.processRadioStat"


mkGcsRadioMessage :: S.RadioStatus -> L.ByteString
mkGcsRadioMessage stat = packPayloadGcsRadioMessage $ GcsRadioMessage
  { rxerrors = S.tx_err    stat
  , fixed    = S.ecc_errs  stat
  , rssi     = S.loc_rssi  stat
  , remrssi  = S.rem_rssi  stat
  , txbuf    = 0
  , noise    = S.loc_noise stat
  , remnoise = S.rem_noise stat
  }

