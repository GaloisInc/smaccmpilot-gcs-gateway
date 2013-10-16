
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
hxencode = undefined

hxdecode :: Console -> Pipe Word8 HXFrame IO ()
hxdecode = undefined

hxframeDebugger :: Console -> String -> Consumer HXFrame IO ()
hxframeDebugger console title = forever $ do
  f <- await
  lift $ consoleDebug console $ msg f
  where
  msg f = printf "%s HXFrame %d [%s]" title (hxframe_tag f)
              (unwords (map hexdig (B.unpack (hxframe_msg f)))) 
  hexdig = printf "0x%0.2x,"

