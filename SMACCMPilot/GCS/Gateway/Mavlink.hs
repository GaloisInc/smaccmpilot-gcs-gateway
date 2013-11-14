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

import           SMACCMPilot.Mavlink.Parser
import           SMACCMPilot.Mavlink.MessageName
import           SMACCMPilot.GCS.Gateway.Monad

import qualified SMACCMPilot.Communications as Comm

--------------------------------------------------------------------------------
-- This is one of the functions that really benefits from pipes/iteratees
-- Instead I'll just mutate some state in the IO monad
mkMavlinkPacketSlice :: IO (ByteString -> GW (Maybe ByteString))
mkMavlinkPacketSlice = do
  stateRef <- newIORef emptyParseSt
  pendingRef <- newIORef []
  return $ \bs -> do
    state <- lift $ readIORef stateRef
    (packets, state') <- run state bs
    lift $ writeIORef stateRef state'
    returnNext pendingRef packets
  where
  run state bs = do
    let (errs, packets, state') = parseStream maxsize state bs
    mapM_ writeErr errs
    return (packets, state')
  maxsize = fromIntegral Comm.mavlinkSize
  returnNext pref newpkts = do
    pending <- lift $ readIORef pref
    case pending ++ newpkts of
      [] -> lift (writeIORef pref [])
          >> return Nothing
      a:as -> do
        lift (writeIORef pref as)
        -- I don't think we'll ever get a bunch of packets backed up in here
        -- but we're better off finding out the easy way
        when (length as > 10) $ warnQueue (length as)
        return (Just a)

  warnQueue len = writeLog ("warning: mavlink packet slicer backlog is "
                           ++ (show len) ++ " long")

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


