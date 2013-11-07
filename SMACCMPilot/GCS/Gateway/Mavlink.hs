module SMACCMPilot.GCS.Gateway.Mavlink
  ( mkMavlinkPacketSlice
  ) where

import Data.IORef

import           Prelude hiding (snd)
import           Control.Monad
import qualified Data.ByteString               as B
import           Data.ByteString               (ByteString)
import           Data.Word
import           Text.Printf

import           SMACCMPilot.Communications (mavlinkSize)
import           SMACCMPilot.Mavlink.Parser
import           SMACCMPilot.Mavlink.MessageName
import           SMACCMPilot.GCS.Gateway.Monad

--------------------------------------------------------------------------------

-- | Take a list of MAVLink packets and pack them until we might exceed the
-- commsec buffer length (mavlinkSize).  Return the packed packets and the rest.
-- Don't try to pack if we don't have at lease enough packets to fill pu the
-- buffer.
takeWhileLen :: [ByteString] -> (ByteString,[ByteString])
takeWhileLen bss
  | B.length (B.concat bss) >= fromIntegral mavlinkSize
  = go B.empty bss 0
  | otherwise
  = (B.empty, bss)
  where
  go snd []     _ = (snd, [])
  go snd (b:bs) i
    | B.length b + i <= fromIntegral mavlinkSize
    = go (snd `B.append` b) bs (B.length b + i)
    | otherwise
    = (snd, b:bs)

-- This is one of the functions that really benefits from pipes/iteratees
-- Instead I'll just mutate some state in the IO monad
-- XXX This could just live in the GW monad in a state component.
mkMavlinkPacketSlice :: IO (String -> ByteString -> GW (Maybe ByteString))
mkMavlinkPacketSlice = do
  stateRef   <- newIORef emptyParseSt  :: IO (IORef ParseSt)
  pendingRef <- newIORef []            :: IO (IORef [ByteString])
  return $ \tag bs -> do
    state <- lift $ readIORef stateRef :: GW ParseSt
    (packets, state') <- run state bs
    lift $ writeIORef stateRef state'
    -- Return the first-queued fully-parsed packet.
    returnNext tag pendingRef packets
  where
  run state bs = do
    let (errs, packets, state') = parseStream maxsize state bs
    mapM_ writeErr errs
    return (packets, state')
  maxsize = fromIntegral mavlinkSize
  returnNext tag pref newpkts = do
    pending <- lift $ readIORef pref
    case pending ++ newpkts of
      []   -> do lift (writeIORef pref [])
                 return Nothing
      pend -> do
        mapM_ (mavlinkDebugger tag) pend
        let (snd, pend') = takeWhileLen pend
        lift (writeIORef pref pend')
        -- I don't think we'll ever get a bunch of packets backed up in here
        -- but we're better off finding out the easy way
        when (length pend' > 10) $ warnQueue (length pend')
        return (Just snd)

  warnQueue len = writeLog ("warning: mavlink packet slicer backlog is "
                           ++ (show len) ++ " long")

mavlinkDebugger :: String -> ByteString -> GW ()
mavlinkDebugger tag bs = writeDbg msg
  where
  p       = B.unpack bs
  msg     = printf "%s MAVLink %s [%s]" tag (pktname p) (display p)
  display = fixup . unwords . map (printf "0x%0.2x,")
  fixup   = reverse . drop 1 . reverse

  pktname :: [Word8] -> String
  pktname (_:_:_:_:_:pid:_) =
    case messagename (fromIntegral pid) of
      Just m -> m
      Nothing -> "??msgid??"
  pktname _ = "(incomplete)"


