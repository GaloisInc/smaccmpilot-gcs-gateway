
module SMACCMPilot.GCS.Gateway.Console
  ( Console
  , consoleLog
  , consoleError
  , consoleDebug
  , newConsole
  ) where

import System.IO
import qualified Control.Concurrent            as C
import qualified Control.Concurrent.STM.TQueue as T
import           Control.Monad
import qualified Control.Monad.STM             as T

import SMACCMPilot.GCS.Gateway.Opts

data ConsoleMsg = ErrorMsg String
                | LogMsg String
                | DebugMsg String
newtype Console = Console { unConsole :: T.TQueue ConsoleMsg }

consoleLog :: Console -> String -> IO ()
consoleLog console s = writeConsole console (LogMsg s)

consoleError :: Console -> String -> IO ()
consoleError console s = writeConsole console (ErrorMsg s)

consoleDebug :: Console -> String -> IO ()
consoleDebug console s = writeConsole console (DebugMsg s)

writeConsole :: Console -> ConsoleMsg -> IO ()
writeConsole c m =
  void $ T.atomically $ T.writeTQueue (unConsole c) m

newConsole :: Options -> Handle -> IO Console
newConsole opts h = do
  conQ <- T.newTQueueIO
  let c = Console conQ
  _ <- C.forkIO $ printerThread c opts h
  return c

printerThread :: Console -> Options -> Handle -> IO ()
printerThread console opts h = forever $ do
  m <- T.atomically $ T.readTQueue conQ
  case m of
    ErrorMsg msg -> when (llevel > 0) $ do
      hPutStrLn h ("ERR: " ++ msg)
    LogMsg msg -> when (llevel > 1) $ do
      hPutStrLn h ("LOG: " ++ msg)
    DebugMsg msg -> when (llevel > 2) $ do
      hPutStrLn h ("DBG: " ++ msg)
  where
  conQ = unConsole console
  llevel = logLevel opts

