
module SMACCMPilot.GCS.Gateway.Opts
  ( Options(..)
  , options
  , defaultOpts
  ) where

import System.Console.GetOpt
import qualified System.Hardware.Serialport as SP

data Options = Options
  { logLevel   :: Integer  -- Logging verbosity
  , serPort    :: FilePath -- Serial Port Filename
  , serBaud    :: SP.CommSpeed -- Serial Baud Rate
  , srvPort    :: Integer  -- Server TCP Port
  , testMode   :: Maybe String
  } deriving (Show)

defaultOpts :: Options
defaultOpts = Options
  { logLevel = 1
  , serPort  = "/dev/ttyUSB0"
  -- If you change default serBaud, also change integer num in options msg
  , serBaud  = SP.CS57600
  , srvPort  = 6000
  , testMode = Nothing
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["quiet"]
      (NoArg (\opts -> opts { logLevel = 0 }))
      "No warning or error reporting"
  , Option [] ["verbose"]
      (NoArg (\opts -> opts { logLevel = 2 }))
      "Full logging output"
  , Option [] ["debug"]
      (NoArg (\opts -> opts { logLevel = 3 }))
      "Full debug output (higher than verbose)"
  , Option [] ["serial"]
      (ReqArg (\arg opts -> opts { serPort = arg }) "filename")
      ("Serial port filename (default: " ++ (serPort defaultOpts) ++ ")")
  , Option [] ["baud"]
      (ReqArg (\arg opts -> opts { serBaud = mkSerBaud arg }) "baudrate")
      ("Serial port baud rate (default:57600")
  , Option [] ["port"]
      (ReqArg (\arg opts -> opts { srvPort = mkSrvPort arg }) "portnumber")
      ("Server TCP port (default:" ++ (show (srvPort defaultOpts)) ++ ")")
  , Option [] ["test"]
      (ReqArg (\arg opts -> opts { testMode = Just arg }) "testname")
      ("test modes (for development only)")
  ]

mkSrvPort :: String -> Integer
mkSrvPort opt =
  maybe (error "Could not parse server port") id (readMaybe opt)

mkSerBaud :: String -> SP.CommSpeed
mkSerBaud opt = maybe (error "invalid serial baud rate") id (commSpeedMaybe csint)
  where
  csint = maybe (error "Could not parse serial baud rate") id (readMaybe opt)
  commSpeedMaybe :: Integer -> Maybe SP.CommSpeed
  commSpeedMaybe 110    = Just SP.CS110
  commSpeedMaybe 300    = Just SP.CS300
  commSpeedMaybe 600    = Just SP.CS600
  commSpeedMaybe 1200   = Just SP.CS1200
  commSpeedMaybe 2400   = Just SP.CS2400
  commSpeedMaybe 4800   = Just SP.CS4800
  commSpeedMaybe 9600   = Just SP.CS9600
  commSpeedMaybe 19200  = Just SP.CS19200
  commSpeedMaybe 38400  = Just SP.CS38400
  commSpeedMaybe 57600  = Just SP.CS57600
  commSpeedMaybe 115200 = Just SP.CS115200
  commSpeedMaybe _      = Nothing

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(x, "")] -> Just x
                _         -> Nothing

