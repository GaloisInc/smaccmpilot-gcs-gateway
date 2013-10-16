
module SMACCMPilot.GCS.Gateway.Commsec
  ( encryptionPipeline
  , decryptionPipeline
  ) where

import           Prelude hiding (log)
import           Control.Monad
import qualified Data.ByteString               as B
import           Data.ByteString               (ByteString)
import           Data.Word

import           Pipes
import           Pipes.Concurrent

import qualified SMACCMPilot.GCS.Commsec         as CS
import qualified SMACCMPilot.GCS.Commsec.Opts    as CS

import           SMACCMPilot.GCS.Gateway.Console

-- XXX this is just a placeholder while I get the types right:
encryptionPipeline :: CS.Options -> Console -> Pipe ByteString ByteString IO ()
encryptionPipeline opts console = forever $ do
  bs <- await
  log ("encryption got" ++ (show (B.unpack bs)))
  yield bs
  where
  log = lift . (consoleLog console)

-- XXX this is just a placeholder while I get the types right:
decryptionPipeline :: CS.Options -> Console -> Pipe ByteString ByteString IO ()
decryptionPipeline opts console = forever $ do
  bs <- await
  log ("decryption got" ++ (show bs))
  yield bs
  where
  log = lift . (consoleLog console)

