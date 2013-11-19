module SMACCMPilot.GCS.Gateway.Mavlink
  ( mkMavlinkPacketSlice
  , mavlinkDebugger
  , mavlinkPacketName
  ) where

import Data.IORef

import           Prelude hiding (snd)
import           Control.Monad
import qualified Data.ByteString               as B
import           Data.ByteString               (ByteString)
import           Data.Word
import           Text.Printf

import           SMACCMPilot.GCS.Mavlink.Parser
import           SMACCMPilot.GCS.Mavlink.MessageName
import           SMACCMPilot.GCS.Gateway.Monad

import qualified SMACCMPilot.Communications as Comm

--------------------------------------------------------------------------------
-- This is one of the functions that really benefits from pipes/iteratees
-- Instead I'll just mutate some state in the IO monad
mkMavlinkPacketSlice :: IO (ByteString -> GW [ByteString])
mkMavlinkPacketSlice = do
  stateRef <- newIORef emptyParseSt
  return $ \bs -> do
    state <- lift $ readIORef stateRef
    (packets, state') <- run state bs
    lift $ writeIORef stateRef state'
    return packets
  where
  run state bs = do
    let (errs, packets, state') = parseStream maxsize state bs
    mapM_ writeErr errs
    return (packets, state')
  maxsize = fromIntegral Comm.mavlinkSize

mavlinkDebugger :: String -> ByteString -> GW ByteString
mavlinkDebugger tag bs = writeDbg msg >> return bs
  where
  p       = B.unpack bs
  msg     = printf "%s MAVLink %s [%s]" tag (mavlinkPacketName p) (display p)
  display = fixup . unwords . map (printf "0x%0.2x,")
  fixup   = reverse . drop 1 . reverse

-- God help us.
mavlinkPacketName :: [Word8] -> String
mavlinkPacketName (_:_:_:_:_:pid:_) =
  case messagename (fromIntegral pid) of
    Just m -> m
    Nothing -> "??msgid??"
mavlinkPacketName _ = "(incomplete)"


