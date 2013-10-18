
module SMACCMPilot.GCS.Gateway.Async where

import           Control.Exception
import qualified Control.Concurrent.Async        as A
import           System.IO

import SMACCMPilot.GCS.Gateway.Console
import SMACCMPilot.GCS.Gateway.Monad

asyncRun :: String -> IO () -> IO (A.Async ())
asyncRun name act = A.async $ catch run c
  where
  run = act >> exit "run complete"
  exit msg = hPutStrLn stderr $ "asyncRun " ++ name ++ " exiting: " ++ msg
  c :: SomeException -> IO ()
  c x = exit ("exception: " ++ show x)

asyncRunGW :: Console -> String -> GW () -> IO (A.Async ())
asyncRunGW console name act = asyncRun name $ runGW (annotate console name) act

