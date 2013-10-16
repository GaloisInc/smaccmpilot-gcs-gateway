
module SMACCMPilot.GCS.Gateway.HXFraming
  ( HXFrame(..)
  , hxframePayloadFromTag
  , createHXFrameWithTag
  , hxframeDebugger

  , hxframedSerial
  ) where

import           Control.Monad
import           Data.ByteString               (ByteString)
import qualified Data.ByteString            as B
import qualified Data.HXStream              as HX
import           Data.Word
import           Pipes
import           Pipes.Concurrent
import           Text.Printf

import SMACCMPilot.GCS.Gateway.Serial
import SMACCMPilot.GCS.Gateway.Console
import SMACCMPilot.GCS.Gateway.Opts

data HXFrame =
  HXFrame
    { hxframe_tag :: Word8
    , hxframe_msg :: ByteString
    }

hxframePayloadFromTag :: Word8 -> Pipe HXFrame ByteString IO ()
hxframePayloadFromTag tag = forever $ do
  f <- await
  when (hxframe_tag f == tag) $ yield (hxframe_msg f)

createHXFrameWithTag :: Word8 -> Pipe ByteString HXFrame IO ()
createHXFrameWithTag tag = forever $ do
  msg <- await
  yield $ HXFrame { hxframe_tag = tag, hxframe_msg = msg }

hxframedSerial :: Options -> Console -> Output HXFrame -> Input HXFrame -> IO ()
hxframedSerial opts console hxoutput hxinput = do
  (fromveh_output, fromveh_input) <- spawn Unbounded
  (toveh_output,   toveh_input)   <- spawn Unbounded
  serialServer opts console fromveh_output toveh_input
  forkEffect $ fromInput hxinput
           >-> hxencode console
           >-> toOutput toveh_output
  forkEffect $ fromInput fromveh_input
           >-> hxdecode console
           >-> toOutput hxoutput


forkEffect :: Effect IO () -> IO ()
forkEffect e = void $ forkIO $ runEffect e

hxencode :: Console -> Pipe HXFrame ByteString IO ()
hxencode _console = forever $ do
  f <- await
  yield $ HX.encode (hxframe_tag f) (hxframe_msg f)
  -- We need to pack the wire with at least trailing FBOs:
  yield $ B.pack [HX.fbo]

hxdecode :: Console -> Pipe Word8 HXFrame IO ()
hxdecode _console = run HX.emptyStreamState
  where
  run state = do
    b <- await
    let (mf, state') = HX.decodeByte b state
    case mf of
      Just (tag, msg) ->
        yield $ HXFrame { hxframe_tag = tag, hxframe_msg = msg}
      Nothing -> return ()
    run state'

hxframeDebugger :: Console -> Pipe HXFrame HXFrame IO ()
hxframeDebugger console = forever $ do
  f <- await
  lift $ consoleDebug console $ msg f
  yield f
  where
  msg f = printf "HXFrame %d [%s]" (hxframe_tag f) (body f)
  body f = fixup (unwords (map hexdig (B.unpack (hxframe_msg f))))
  hexdig = printf "0x%0.2x,"
  -- Drop last char because the above map/unwords is bad hack
  fixup = reverse . drop 1 . reverse

