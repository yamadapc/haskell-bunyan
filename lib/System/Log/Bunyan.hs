{-# LANGUAGE OverloadedStrings #-}
module System.Log.Bunyan
    ( -- * Creating a logger
      newLogger
    , Logger(..)
      -- * Logging functions
    , llog
    , lfatal
    , lerror
    , lwarn
    , linfo
    , ldebug
    , ltrace
      -- * Re-exports
    , module Data.Aeson
    , module Data.Default
    , HostName
    , LogStr
    , ProcessID
    ) where

import           Data.Aeson
import           Data.Aeson.Types      (Pair)
import           Data.Default
import           Data.Text             (Text)
import           Data.Time
import           Data.Time.ISO8601
import           Network.HostName
import           System.Log.FastLogger
import           System.Posix          (ProcessID, getProcessID)
import           System.Posix.Types    (CPid (..))

newLogger :: Logger -> IO Logger
newLogger logger = do
    set <- newStdoutLoggerSet defaultBufSize
    pid <- getProcessID
    hostname <- getHostName
    return $ logger { loggerPushLn = pushLogStrLn set
                    , loggerPid = pid
                    , loggerHostname = hostname
                    }

data Logger = Logger { loggerPushLn   :: LogStr -> IO ()
                     , loggerName     :: String
                     , loggerPid      :: ProcessID
                     , loggerHostname :: HostName
                     , loggerVersion  :: Int
                     }

instance Default Logger where
    def = Logger { loggerPushLn = const $ error
                                  "Please use `newLogger` to create a logger"
                 , loggerName = ""
                 , loggerPid = 0
                 , loggerHostname = ""
                 , loggerVersion = 0
                 }

llog :: Logger -> Level -> [Pair] -> Text -> IO ()
llog logger level ps msg = do
    tm <- formatISO8601Millis <$> getCurrentTime
    let CPid ipid = loggerPid logger
        ps' = "level" .= level :
              "v" .= (0 :: Int) :
              "hostname" .= (loggerHostname logger) :
              "pid" .= ipid :
              "name" .= (loggerName logger) :
              "time" .= tm :
              "msg" .= msg :
              ps
    loggerPushLn logger $ toLogStr $ encode (object ps')

lfatal, lerror, lwarn, linfo, ldebug, ltrace :: Logger -> [Pair] -> Text -> IO ()
lfatal logger = llog logger levelFatal
lerror logger = llog logger levelError
lwarn logger = llog logger levelWarn
linfo logger = llog logger levelInfo
ldebug logger = llog logger levelDebug
ltrace logger = llog logger levelTrace

type Level = Int

levelFatal, levelError, levelWarn, levelInfo, levelDebug, levelTrace :: Level
levelFatal = 60
levelError = 50
levelWarn = 40
levelInfo = 30
levelDebug = 20
levelTrace = 10
