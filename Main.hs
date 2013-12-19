
module Main where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import qualified SMACCMPilot.GCS.Commsec.Opts as CS
import qualified SMACCMPilot.GCS.Gateway.Opts as App
import           SMACCMPilot.GCS.Gateway.Server

main :: IO ()
main = do
  args <- getArgs
  (csopts, appopts) <- getOpts args
  gatewayServer csopts appopts
  exitSuccess

getOpts :: [String] -> IO (CS.Options, App.Options)
getOpts argv = do
  case getOpt' Permute CS.options argv of
    (csopts, nonOpts, unrecOpts, errs)
      | not (null errs) -> usage ("Invalid options: "
                                  ++ (unwords errs))
      | not (null nonOpts) -> usage ("Invalid arguments: "
                                  ++ (unwords nonOpts))
      | otherwise ->
          case getOpt' Permute App.options unrecOpts of
            (appopts, _nonOpts', unrecOpts', errs')
              | not (null errs) -> usage ("Invalid options: "
                                          ++ (unwords errs'))
              | not (null unrecOpts') -> usage ("Unknown options: "
                                                ++ (unwords unrecOpts'))
              | otherwise -> return
                  ( foldl (flip id) CS.defaultOpts csopts
                  , foldl (flip id) App.defaultOpts appopts
                  )

usage :: String -> IO a
usage errs = do
  hPutStr stderr (appui ++ "\n" ++ csui)
  exitFailure
  where
  header = "smaccm-gcs-gateway\n" ++ errs
  appui = usageInfo header App.options
  csui = usageInfo "" CS.options
