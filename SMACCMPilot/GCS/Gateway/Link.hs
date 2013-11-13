
module SMACCMPilot.GCS.Gateway.Link
  ( linkManagment
  ) where

import           Control.Monad
import qualified Control.Concurrent              as C
import qualified Data.ByteString                 as B
import           Data.ByteString (ByteString)
import           Data.Char (ord)
import           Data.Word

import           SMACCMPilot.GCS.Gateway.Monad
import           SMACCMPilot.GCS.Gateway.Async
import           SMACCMPilot.GCS.Gateway.Queue
import           SMACCMPilot.GCS.Gateway.Console
import           SMACCMPilot.GCS.Gateway.HXFraming
import           SMACCMPilot.GCS.Gateway.ByteString
import           SMACCMPilot.GCS.Gateway.RadioDebug
import           SMACCMPilot.GCS.Gateway.RadioStatus


linkManagment :: Console -> QueueOutput HXFrame -> QueueInput HXFrame -> IO ()
linkManagment console link_output link_input = do
  void $ asyncRunGW console "link process" $ forever $ do
    queuePopGW link_input >>= ((hxframePayloadFromTag 1) >~> process)
  void $ asyncRunGW console "link request" $ forever $ do
    lift $ C.threadDelay 1000000
    createHXFrameWithTag 1 statReqFrame >>= queuePushGW link_output
--    lift $ C.threadDelay 1000000
--    createHXFrameWithTag 1 dbgReqFrame >>= queuePushGW link_output
  where
  statReqFrame = B.pack [0x42, 0x0d] -- 'B'
  dbgReqFrame  = B.pack [0x44, 0x0d] -- 'D'

  process :: ByteString -> GW ()
  process frame = -- bytestringDebugger "raw" frame >>
    case tag of
      0x42 -> processRadioStat  frame
      0x44 -> processRadioDebug frame
      _    -> processRadioOther frame
    where tag = head (B.unpack frame)

  processRadioStat  f = case parseRadioStatus f of
                          Just s -> debugRadioStatus s
                          Nothing -> writeDbg "Bad radio stat packet"
  processRadioDebug f = case parseRadioDbg f of
                          Just s -> debugRadioDbg s
                          Nothing -> writeDbg "Bad radio debug packet"
  processRadioOther f = bytestringDebugger "Unknown packet" f >> return ()

