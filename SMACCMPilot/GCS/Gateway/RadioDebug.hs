{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.GCS.Gateway.RadioDebug where

import qualified Data.ByteString                 as B
import           Data.ByteString (ByteString)
import           Data.Char (ord)
import           Data.Word

import           SMACCMPilot.GCS.Gateway.Monad

data RadioDbg =
  RadioDbg
    { sik       :: Bool   -- Stored IBool
    , loc_rssi  :: Word8  -- Stored Uint8
    , loc_noise :: Word8  -- Stored Uint8
    , loc_rxctr :: Word16 -- Stored Uint16
    , rem_rssi  :: Word8  -- Stored Uint8
    , rem_noise :: Word8  -- Stored Uint8
    , rem_rxctr :: Word16 -- Stored Uint16
    , tx_err    :: Word16 -- Stored Uint16
    , rx_err    :: Word16 -- Stored Uint16
    , tx_ovf    :: Word16 -- Stored Uint16
    , tx_ok     :: Word16 -- Stored Uint16
    , rx_ovf    :: Word16 -- Stored Uint16
    , rx_ok     :: Word16 -- Stored Uint16
    , ecc_errs  :: Word16 -- Stored Uint16
    , ecc_pkts  :: Word16 -- Stored Uint16
    , max_xmit  :: Word8  -- Stored Uint8
    , tx_insert :: Word8  -- Stored Uint8
    , tx_remove :: Word8  -- Stored Uint8
    , rx_insert :: Word8  -- Stored Uint8
    , rx_remove :: Word8  -- Stored Uint8
    } deriving (Show)

parseRadioDbg :: ByteString -> Maybe RadioDbg
parseRadioDbg bs =
  if B.length bs /= 30 then Nothing
  else if fromIntegral (head bss) /= (ord 'D') then Nothing
  else Just unpacked
  where
  bss = B.unpack bs
  v8 off = bss !! off
  v16 o1 o2 = ((fromIntegral (v8 o1)) * 256) + (fromIntegral (v8 o2))
  unpacked = RadioDbg
    { sik       = True
    , loc_rssi  = v8 1
    , loc_noise = v8 2
    , loc_rxctr = v16 3 4
    , rem_rssi  = v8 5
    , rem_noise = v8 6
    , rem_rxctr = v16 7 8
    , tx_err    = v16 9 10
    , rx_err    = v16 11 12
    , tx_ovf    = v16 13 14
    , rx_ovf    = v16 15 16
    , ecc_errs  = v16 17 18
    , ecc_pkts  = v16 19 20
    , tx_ok     = v16 21 22
    , rx_ok     = v16 23 24
    , max_xmit  = v8 25
    , tx_insert = v8 26
    , tx_remove = v8 27
    , rx_insert = v8 28
    , rx_remove = v8 29
    }

filterRadioDbg :: ByteString -> GW (Maybe RadioDbg)
filterRadioDbg bs = return $ parseRadioDbg bs

debugRadioDbg :: RadioDbg -> GW ()
debugRadioDbg rs = writeDbg (show rs)

