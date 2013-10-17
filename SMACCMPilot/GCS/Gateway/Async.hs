
module SMACCMPilot.GCS.Gateway.Async where

import           Control.Exception
import qualified Control.Concurrent.Async        as A
import           Pipes
import           Pipes.Concurrent
import           System.IO

asyncEffect :: String -> Effect IO () -> IO (A.Async ())
asyncEffect name e = A.async $ catch run c
  where
  run = do
    runEffect e
    performGC
    exit "effect complete"
  exit msg = hPutStrLn stderr $ "asyncEffect " ++ name ++ " exiting: " ++ msg
  c :: SomeException -> IO ()
  c x = exit ("exception: " ++ show x)

