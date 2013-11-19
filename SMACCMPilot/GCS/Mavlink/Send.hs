
module SMACCMPilot.GCS.Mavlink.Send where

import SMACCMPilot.Mavlink.Messages (messageLensCRCs)


import SMACCMPilot.GCS.Mavlink.CRC

import           Data.ByteString.Lazy         (ByteString)
import qualified Data.ByteString.Lazy         as B
import qualified Data.ByteString.Lazy.Builder as B
import           Data.Monoid
import           Data.Word

data MavlinkState =
  MavlinkState
    { seqnum :: Word8
    , sysid  :: Word8
    , compid :: Word8
    }

mavlinkSend :: MavlinkState -> Word8 -> ByteString -> Maybe (ByteString, MavlinkState)
mavlinkSend state msgid packedpayload = case lookup msgid messageLensCRCs of
  Just (len, crcextra) -> case B.length packedpayload + 6 == fromIntegral len of
    True -> Just (B.toLazyByteString (packet len crcextra), state')
    False -> Nothing
  Nothing -> Nothing
  where
  state' = state { seqnum = 1 + seqnum state }
  packet len crcextra = B.word8 254 <> payload len <> checksum (payload len) crcextra
  checksum b crcextra = B.word8 lo <> B.word8 hi
    where
    (lo, hi) = crc_lo_hi $ foldl crc_accumulate crc_init_v bytes
    bytes = B.unpack (B.toLazyByteString (b <> B.word8 crcextra))
  payload len = B.word8 len
             <> B.word8 (seqnum state)
             <> B.word8 (sysid  state)
             <> B.word8 (compid state)
             <> B.word8 msgid
             <> B.lazyByteString packedpayload -- XXX ADD HEADER STUFF HERE

