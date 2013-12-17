
module SMACCMPilot.GCS.Gateway.Commsec
  ( mkCommsec
  , Commsec
  , encrypt
  , decrypt
  ) where

import qualified Data.ByteString               as B
import           Data.ByteString               (ByteString)
import           Text.Printf

import qualified SMACCMPilot.Communications      as Comm
import qualified SMACCMPilot.GCS.Commsec         as CS
import qualified SMACCMPilot.GCS.Commsec.Opts    as CS
import           SMACCMPilot.GCS.Gateway.Monad



data Commsec =
  Commsec
    { encrypt :: ByteString -> GW (Maybe ByteString)
    , decrypt :: ByteString -> GW (Maybe ByteString)
    }

mkCommsec :: CS.Options -> IO Commsec
mkCommsec opts = do
  cryptoCtx <- CS.secPkgInit_HS baseId rsalt rkey ssalt skey
  return $ Commsec
    { encrypt = \pt ->
        case ptlen == B.length pt of
          False -> invalid "encrypt pt length" ptlen (B.length pt)
          True -> do
            mct <- lift $ CS.secPkgEncInPlace_HS cryptoCtx pt
            case mct of
              Nothing -> err "encryption failure"
              Just ct -> case (B.length ct) == ctlen of
                False -> invalid "encrypt ct length" ctlen (B.length ct)
                True -> return (Just ct)

    , decrypt = \ct ->
        case (B.length ct) == ctlen of
          False -> invalid "decrypt ct length" ctlen (B.length ct)
          True -> do
            decrypted <- lift $ CS.secPkgDec_HS cryptoCtx ct
            case decrypted of
              Left e -> err ("decryption error: " ++ (show e))
              Right pt -> case (B.length pt) == ptlen of
                False -> invalid "decrypt pt length" ptlen (B.length pt)
                True -> return (Just pt)
    }
  where
  ptlen   = fromIntegral Comm.mavlinkSize
  ctlen   = fromIntegral Comm.commsecPkgSize
  baseId  = fromIntegral (CS.sendID opts)
  rsalt   = CS.recvSalt opts
  rkey    = B.pack (CS.recvKey opts)
  ssalt   = CS.sendSalt opts
  skey    = B.pack (CS.sendKey opts)

  invalid name expected got = err $ printf "Invalid %s: expected %s got %s"
                                  name (show expected) (show got)
  err msg = writeErr msg >> return Nothing

