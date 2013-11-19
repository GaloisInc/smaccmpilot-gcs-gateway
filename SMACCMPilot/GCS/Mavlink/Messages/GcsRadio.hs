
module SMACCMPilot.GCS.Mavlink.Messages.GcsRadio where

import           Data.ByteString.Lazy         (ByteString)
import qualified Data.ByteString.Lazy         as B
import qualified Data.ByteString.Lazy.Builder as B
import           Data.Monoid
import           Data.Word

data GcsRadioMessage =
  GcsRadioMessage
    { rxerrors :: Word16
    , fixed    :: Word16
    , rssi     :: Word8
    , remrssi  :: Word8
    , txbuf    :: Word8
    , noise    :: Word8
    , remnoise :: Word8
    }

packPayloadGcsRadioMessage :: GcsRadioMessage -> ByteString
packPayloadGcsRadioMessage m = B.toLazyByteString builder
  where
  builder = B.word16BE (rxerrors m)
         <> B.word16BE (fixed    m)
         <> B.word8    (rssi     m)
         <> B.word8    (remrssi  m)
         <> B.word8    (txbuf    m)
         <> B.word8    (noise    m)
         <> B.word8    (remnoise m)

