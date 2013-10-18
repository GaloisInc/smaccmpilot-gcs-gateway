
module SMACCMPilot.GCS.Gateway.HXFraming
  ( HXFrame(..)
  , hxframePayloadFromTag
  , createHXFrameWithTag
  , hxframeDebugger
  , hxframedSerial
  ) where

import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString            as B
import qualified Data.HXStream              as HX
import           Data.Word
import           Text.Printf

import SMACCMPilot.GCS.Gateway.Serial
import SMACCMPilot.GCS.Gateway.Console
import SMACCMPilot.GCS.Gateway.Opts
import SMACCMPilot.GCS.Gateway.Async
import SMACCMPilot.GCS.Gateway.Monad
import SMACCMPilot.GCS.Gateway.Queue

data HXFrame =
  HXFrame
    { hxframe_tag :: Word8
    , hxframe_msg :: ByteString
    }

hxframePayloadFromTag :: Word8 -> HXFrame -> GW (Maybe ByteString)
hxframePayloadFromTag tag f
  | (hxframe_tag f == tag) = return (Just (hxframe_msg f))
  | otherwise = return Nothing

createHXFrameWithTag :: Word8 -> ByteString -> GW HXFrame
createHXFrameWithTag tag msg =
  return $ HXFrame { hxframe_tag = tag, hxframe_msg = msg }

hxframedSerial :: Options -> Console -> [QueueOutput HXFrame] -> QueueInput HXFrame -> IO ()
hxframedSerial opts console hxoutputs hxinput = do
  (fromveh_output, fromveh_input) <- newQueue
  (toveh_output,   toveh_input)   <- newQueue
  serialServer opts console fromveh_output toveh_input
  void $ asyncRunGW console "hxstream encode" $ forever
      $ queuePopGW hxinput >>= (hxencode >=> queuePushGW toveh_output)
  void $ asyncRunGW console "hxstream decode" $
    decoder HX.emptyStreamState fromveh_input
  where
  decoder ss inqueue = do
    b <- queuePopGW inqueue
    (ss', mf) <- hxdecode ss b
    case mf of
      Nothing -> decoder ss' inqueue
      Just f -> do
        forM_ hxoutputs $ \out -> queuePushGW out f
        decoder ss' inqueue

hxencode :: HXFrame -> GW ByteString
hxencode f = return $ B.pack extraFBO
  where
  packed = HX.encode (hxframe_tag f) (hxframe_msg f)
  -- We need to pack the wire with at least trailing FBOs:
  extraFBO = (B.unpack packed) ++ [HX.fbo]

hxdecode :: HX.StreamState -> Word8 -> GW (HX.StreamState, Maybe HXFrame)
hxdecode state b = return (state', hxf `fmap` mf)
    where
    (mf, state') = HX.decodeByte b state
    hxf (tag, msg) = HXFrame { hxframe_tag = tag, hxframe_msg = msg}

hxframeDebugger :: String -> HXFrame -> GW HXFrame
hxframeDebugger tag f = writeDbg msg >> return f
  where
  msg = printf "%s HXFrame %d [%s]" tag (hxframe_tag f) body
  body = fixup (unwords (map hexdig (B.unpack (hxframe_msg f))))
  hexdig = printf "0x%0.2x,"
  -- Drop last char because the above map/unwords is bad hack
  fixup = reverse . drop 1 . reverse

