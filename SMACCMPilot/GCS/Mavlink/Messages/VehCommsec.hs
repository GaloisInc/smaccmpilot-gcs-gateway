{-# LANGUAGE OverloadedStrings #-}

-- Written by-hand for now... XXX Doesn't perform CRC checks.
-- Pleasantly show statistics from SMACCMPilot decrypting GCS data.

module SMACCMPilot.GCS.Mavlink.Messages.VehCommsec
  ( vehCommsecInfo
  ) where

import qualified Data.ByteString.Char8        as B
import qualified SMACCMPilot.Communications   as C
import           Data.Monoid
import           Data.Word
import           Data.Bits

-- Must match message smaccm-mavlink message payload VEH_COMMSEC.
data VehCommsecMessage =
  VehCommsecMessage
    { time       :: Word32
    , goodMsgs   :: Word32
    , badMsgs    :: Word32
    , err        :: Word8
    }
  deriving (Show, Read, Eq)

gcsVehCommsecId :: Word8
gcsVehCommsecId = 185

-- Mavlink-specific stuff.
hdrLen, crcLen, payloadLen :: Int
hdrLen     = 6
crcLen     = 2
payloadLen = 13

prettyShowMsg :: VehCommsecMessage -> [B.ByteString]
prettyShowMsg (VehCommsecMessage t gm bm e) =
  [ "ms since last good msg: " <> mkTime
  , "good msgs received:     " <> p gm
  , "bad msgs received:      " <> p bm
  , "current error msg:      " <> mkErr
  ]
  where
  p = B.pack . show
  mkErr  = B.pack $ C.showCommsecErrors (fromIntegral e :: Integer)
  mkTime = p t

-- A proper GCS Commsec info message?
mavlinkMsgToData :: [Word8] -> Maybe [Word8]
mavlinkMsgToData bs
  |  length bs == hdrLen + crcLen + payloadLen
  && bs !! 5 == gcsVehCommsecId
  = Just $ take payloadLen (drop hdrLen bs)
  | otherwise
  = Nothing

payloadToData :: [Word8] -> VehCommsecMessage
payloadToData payload = VehCommsecMessage
  { time     = parseLE t
  , goodMsgs = parseLE gm
  , badMsgs  = parseLE bm
  , err      = parseLE e
  }
  where
  t  = extract (0,4)   payload
  gm = extract (4,8)   payload
  bm = extract (8,12)  payload
  e  = extract (12,13) payload

-- Take values at indexes >= start and < end
extract :: (Int, Int) -> [Word8] -> [Word8]
extract (start, end) bs = drop start (take end bs)

-- Parse little Endian, one byte at time.
parseLE :: (Num a, Bits a) => [Word8] -> a
parseLE = fst . foldl go (0, 0)
  where
  go :: (Num a, Bits a) => (a, Int) -> Word8 -> (a, Int)
  go (acc, i) b = (shiftL (fromIntegral b) (i * 8) + acc, i+1)

vehCommsecInfo :: [Word8] -> [B.ByteString]
vehCommsecInfo bs = case mavlinkMsgToData bs of
  Nothing      -> []
  Just payload -> prettyShowMsg (payloadToData payload)
