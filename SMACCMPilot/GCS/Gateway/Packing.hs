
module SMACCMPilot.GCS.Gateway.Packing
  ( mkPacker
  ) where

import Data.IORef

import           Prelude hiding (snd)
import           Control.Monad
import qualified Data.ByteString               as B
import           Data.ByteString               (ByteString)
import           Data.Word
import           Text.Printf

import           SMACCMPilot.GCS.Gateway.Monad
import           SMACCMPilot.GCS.Gateway.Mavlink

mkPacker :: Integer -> IO (ByteString -> GW (Maybe ByteString))
mkPacker packsize = do
  stateRef <- newIORef []
  return $ \bs -> do
    state <- lift $ readIORef stateRef
    let (packed, state') = run state bs
    lift $ writeIORef stateRef state'
    return packed
  where
  run :: [ByteString] -> ByteString -> (Maybe ByteString, [ByteString])
  run pending latest = if mavlinkPacketName (B.unpack latest) == "HEARTBEAT"
                       || sum (map B.length all) > fromIntegral packsize
     then (Just packed, rest)
     else (Nothing, all)
    where
    (packed, rest) = takeWhileLen packsize all
    all = pending ++ [latest]

-- | Take a list of sub-frames and pack them until we might exceed the frame
-- length.  Return the concatenated sub-frames, which will have length less than
-- or equal to the packsize, and the remainder of the input.

takeWhileLen :: Integer -> [ByteString] -> (ByteString, [ByteString])
takeWhileLen packsize bss = go B.empty bss 0
  where
  go :: ByteString    -- To be sent, accumulator in recursion
     -> [ByteString]  -- Pending being sent
     -> Int           -- Index. Invariant: equal to B.length snd
     -> (ByteString, [ByteString]) -- Packed, remaining
  go snd []     _ = (snd, [])
  go snd (b:bs) i
    | B.length b + i <= fromIntegral packsize
    = go (snd `B.append` b) bs (B.length b + i)
    | otherwise
    = (snd, b:bs)


