
module SMACCMPilot.GCS.Gateway.Mavlink
  ( mavlinkPacketSlice
  , mavlinkDebugger
  ) where

import           Control.Monad
import qualified Data.ByteString               as B
import           Data.ByteString               (ByteString)
import           Data.Word
import           Text.Printf

import           Pipes

import           SMACCMPilot.Communications (mavlinkSize)
import           SMACCMPilot.Mavlink.Parser
import           SMACCMPilot.Mavlink.MessageName
import           SMACCMPilot.GCS.Gateway.Console

mavlinkPacketSlice :: Console -> Pipe ByteString ByteString IO ()
mavlinkPacketSlice console = run emptyParseSt
  where
  run state = do
    bs <- await
    let (errs, packets, state') = parseStream maxsize state bs
    unless (null errs) $ mapM_ err errs
    mapM_ yield packets
    run state'
  err  = lift . (consoleError console)
  maxsize = fromIntegral mavlinkSize

mavlinkDebugger :: Console -> Pipe ByteString ByteString IO ()
mavlinkDebugger console = forever $ do
  bs <- await
  let p = B.unpack bs
  dbg $ printf "MAVLink %s [%s]" (pktname p) (display p)
  yield bs
  where
  dbg msg = lift $ consoleDebug console msg
  display p = fixup $ unwords $ map (printf "0x%0.2x,") p
  fixup = reverse . drop 1 . reverse

  pktname :: [Word8] -> String
  pktname (_:_:_:_:_:pid:_) =
    case messagename (fromIntegral pid) of
      Just m -> m
      Nothing -> "??msgid??"
  pktname _ = "(incomplete)"


