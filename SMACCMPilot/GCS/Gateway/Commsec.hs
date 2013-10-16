
module SMACCMPilot.GCS.Gateway.Commsec
  ( mkCommsec
  , Commsec
  , encrypt
  , decrypt
  ) where

import           Control.Monad
import qualified Data.ByteString               as B
import           Data.ByteString               (ByteString)
import           Text.Printf

import           Pipes

import qualified SMACCMPilot.Communications      as Comm
import qualified SMACCMPilot.GCS.Commsec         as CS
import qualified SMACCMPilot.GCS.Commsec.Opts    as CS

import           SMACCMPilot.GCS.Gateway.Console


data Commsec =
  Commsec
    { encrypt :: Pipe ByteString ByteString IO ()
    , decrypt :: Pipe ByteString ByteString IO ()
    }

mkCommsec :: CS.Options -> Console -> IO Commsec
mkCommsec opts console = do
  cryptoCtx <- CS.secPkgInit_HS baseId rsalt rkey ssalt skey
  return $ Commsec
    { encrypt = forever $ do
        pt <- await
        case ptlen == B.length pt of
          False -> invalid "encrypt pt length" ptlen (B.length pt)
          True -> do
            mct <- lift $ CS.secPkgEncInPlace_HS cryptoCtx pt
            case mct of
              Nothing -> err "encryption failure"
              Just ct -> case (B.length ct) == ctlen of
                False -> invalid "encrypt ct length" (B.length ct) ctlen
                True -> yield ct

    , decrypt = forever $ do
        ct <- await
        case (B.length ct) == ctlen of
          False -> invalid "decrypt ct length" (B.length ct) ctlen
          True -> do
            decrypted <- lift $ CS.secPkgDec_HS cryptoCtx ct
            case decrypted of
              Left e -> err ("decryption error: " ++ (show e))
              Right pt -> case (B.length pt) == ptlen of
                False -> invalid "decrypt pt length" (B.length pt) ptlen
                True -> yield pt
    }
  where
  ptlen   = fromIntegral Comm.mavlinkSize
  ctlen   = fromIntegral Comm.commsecPkgSize
  baseId  = fromIntegral (CS.sendID opts)
  rsalt   = CS.recvSalt opts
  rkey    = B.pack (CS.recvKey opts)
  ssalt   = CS.sendSalt opts
  skey    = B.pack (CS.sendKey opts)

  invalid name a b = err $ printf "Invalid %s: expected %s got %s"
                                        name (show a) (show b)
  err = lift . (consoleError console)

