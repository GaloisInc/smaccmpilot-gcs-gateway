{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.GCS.Gateway.Monad
  ( GW
  , runGW
  , lift
  , writeErr
  , writeLog
  , writeDbg
  , queuePushGW
  , queuePopGW
  , (>~>)
  , (>*>)
  , (>>~)
  ) where

import qualified MonadLib as M

import SMACCMPilot.GCS.Gateway.Console
import SMACCMPilot.GCS.Gateway.Queue

newtype GW a =
  GW { unGW :: M.ReaderT Console IO a }
  deriving (Functor, Monad)

runGW :: Console ->  GW a -> IO a
runGW console gw = M.runReaderT console (unGW gw)

lift :: IO a -> GW a
lift = GW . M.lift

getConsole :: GW Console
getConsole = GW M.ask

writeErr :: String -> GW ()
writeErr msg = do
  c <- getConsole
  lift $ consoleError c msg

writeDbg :: String -> GW ()
writeDbg msg = do
  c <- getConsole
  lift $ consoleDebug c msg

writeLog :: String -> GW ()
writeLog msg = do
  c <- getConsole
  lift $ consoleLog c msg

queuePopGW :: QueueInput a -> GW a
queuePopGW q = lift (queuePop q)

queuePushGW :: QueueOutput a -> a -> GW ()
queuePushGW q v = lift (queuePush q v)

infixr 0 >~>
(>~>) :: Monad m => (a -> m (Maybe b)) -> (b -> m ()) -> a -> m ()
(>~>) a b a1 = a a1 >>= maybe (return ()) b

infixr 0 >*>
(>*>) :: Monad m => (a -> m [b]) -> (b -> m ()) -> a -> m ()
(>*>) a b a1 = a a1 >>= mapM_ b

infixr 0 >>~
(>>~) :: Monad m => (m (Maybe b)) -> (b -> m ()) -> m ()
(>>~) a b = ((\_ -> a) >~> b) ()
