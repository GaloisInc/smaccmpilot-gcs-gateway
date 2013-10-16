{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.GCS.Gateway.Link
  ( linkManagment
  , RadioStat(..)
  ) where

import           Control.Monad
import qualified Control.Concurrent              as C
import qualified Data.ByteString                 as B
import           Data.ByteString (ByteString)
import           Data.Char (ord)
import           Data.Word

import           Pipes
import           Pipes.Concurrent

import           SMACCMPilot.GCS.Gateway.Console
import           SMACCMPilot.GCS.Gateway.HXFraming
import           SMACCMPilot.GCS.Gateway.ByteString

data RadioStat =
  RadioStat
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
    , rx_ovf    :: Word16 -- Stored Uint16
    , ecc_errs  :: Word16 -- Stored Uint16
    , ecc_pkts  :: Word16 -- Stored Uint16
    } deriving (Show)

parseRadioStat :: ByteString -> Maybe RadioStat
parseRadioStat bs =
  if B.length bs /= 21 then Nothing
  else if fromIntegral (head bss) /= (ord 'B') then Nothing
  else Just unpacked
  where
  bss = B.unpack bs
  v8 off = bss !! off
  v16 o1 o2 = ((fromIntegral (v8 o1)) * 256) + (fromIntegral (v8 o2))
  unpacked = RadioStat
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
    }

filterRadioStat :: Pipe ByteString RadioStat IO ()
filterRadioStat = forever $ do
  bs <- await
  case parseRadioStat bs of
    Just rs -> yield rs
    Nothing -> return ()

debugRadioStat :: Console -> Pipe RadioStat RadioStat IO ()
debugRadioStat console = forever $ do
  rs <- await
  dbg (show rs)
  yield rs
  where
  dbg = lift . (consoleDebug console)

linkManagment :: Console -> Output HXFrame -> Input HXFrame -> IO ()
linkManagment console link_output link_input = do
  forkEffect $ producePeriodically 1000000 (B.pack [0x42, 0x0d])
           >-> bytestringDebugger (annotate console "toradio")
           >-> createHXFrameWithTag 1
           >-> toOutput link_output

  forkEffect $ fromInput link_input
           >-> hxframePayloadFromTag 1
           >-> bytestringDebugger (annotate console "fromradio")
           >-> filterRadioStat
           >-> debugRadioStat (annotate console "fromradio")
           >-> consumeAll
  where
  producePeriodically per val = forever $
    (lift (C.threadDelay per)) >> yield val
  consumeAll = forever (await >>= const (return ()))

  forkEffect :: Effect IO () -> IO ()
  forkEffect e = void $ forkIO $ runEffect e




