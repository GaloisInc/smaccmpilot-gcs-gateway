

module SMACCMPilot.GCS.Gateway.ByteString where

import           Control.Monad
import           Data.ByteString               (ByteString)
import qualified Data.ByteString            as B
import           Pipes
import           Text.Printf

import SMACCMPilot.GCS.Gateway.Serial
import SMACCMPilot.GCS.Gateway.Serial
import SMACCMPilot.GCS.Gateway.Console
import SMACCMPilot.GCS.Gateway.Console

-- Pad out Mavlink packets.
-- paddedPacket :: B.ByteString -> B.ByteString
-- paddedPacket bs =
--   bs `B.append` (B.pack $ replicate (fromInteger C.mavlinkSize - B.length bs) 0)
--

bytestringPad :: Integer -> Pipe ByteString ByteString IO ()
bytestringPad lint = forever $ do
  bs <- await
  when (B.length bs <= len) $
    yield $ bs `B.append` (B.pack $ replicate (len - B.length bs) 0)
  where
  len = fromInteger lint

bytestringDebugger :: Console -> Pipe ByteString ByteString IO ()
bytestringDebugger console = forever $ do
  f <- await
  lift $ consoleDebug console $ msg f
  yield f
  where
  msg f = printf "ByteString %d [%s]" (B.length f) (body f)
  body f = fixup (unwords (map hexdig (B.unpack f)))
  hexdig = printf "0x%0.2x,"
  -- Drop last char because the above map/unwords is bad hack
  fixup = reverse . drop 1 . reverse


