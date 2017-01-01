{-# LANGUAGE OverloadedStrings #-}

{- |
   Module     : System.Log.Handler.Graylog
   Copyright  : Copyright (C) 2017 Stefan Schmidt
   License    : BSD3

   Maintainer : Stefan Schmidt <stefanschmidt42@googlemail.com> 
   Stability  : provisional
   Portability: portable

Graylog handler for the Haskell Logging Framework

Written by Stefan Schmidt, stefanschmidt42\@googlemail.com

-}

module System.Log.Handler.Graylog
    ( GraylogHandler
    , openlog

    -- re-export from Graylog.UDP
    , Field
    , gelfSField
    , gelfNField
    )
where 

import qualified Data.Text as T

import Network.HostName (getHostName)

import System.Log
import System.Log.Handler
import System.Log.Formatter
import Graylog.UDP

data GraylogHandler = GraylogHandler
    { gh_ownHostName :: T.Text
    , gh_priority    :: Priority
    , gh_formatter   :: LogFormatter GraylogHandler
    , gh_graylog     :: Graylog
    , gh_attributes  :: [Field]
    }

openlog :: String                       -- ^ hostname or ip address
        -> String                       -- ^ portnumber (e.g. 12201)
        -> Priority                     -- ^ messages logged below this priority will be ignored.  To include every message, set this to 'DEBUG'.
        -> [Field]                      -- ^ additional graylog fields
        -> IO GraylogHandler            -- ^ returns the new handler
openlog hostname portnumber priority fields
  = do
    ownName <- getHostName
    eglog <- openGraylog hostname portnumber defaultChunkSize
    case eglog of
        Left  e -> error e
        Right g -> return $ GraylogHandler (T.pack ownName) priority graylogFormatter g fields

graylogFormatter :: LogFormatter GraylogHandler
graylogFormatter = simpleLogFormatter "$loggername/$pid/$tid - $msg"

hsLoggerPrioToGelfPrio :: Priority -> SyslogLevel
hsLoggerPrioToGelfPrio p
  = case p of
      DEBUG     -> Debug
      INFO      -> Informational
      NOTICE    -> Notice
      WARNING   -> Warning
      ERROR     -> Error
      CRITICAL  -> Critical
      ALERT     -> Alert
      EMERGENCY -> Emergency  

createGelf :: GraylogHandler
           -> Priority
           -> String
           -> GELF
createGelf gh prio msg = GELF
    { _gelfVersion      = Version1x1
    , _gelfHost         = (gh_ownHostName gh)
    , _gelfShortMessage = (T.pack $ msg)
    , _gelfFullMessage  = Nothing
    , _gelfTimestamp    = Nothing
    , _gelfLevel        = Just $ hsLoggerPrioToGelfPrio prio
    , _gelfLine         = Nothing
    , _gelfFile         = Nothing
    , _gelfAdditionals  = (gh_attributes gh)
--    [ gelfSField "type" "onex-haskell"
--                          , gelfSField "lhotse_team" "shopoffice"
--                          , gelfSField "lhotse_vertical" "shoppages"
--                          , gelfSField "lhotse_group" "develop" -- configure this !!!
--                         ]
    }

instance LogHandler GraylogHandler where
    setLevel gh p = gh{gh_priority = p}
    getLevel gh = gh_priority gh
    setFormatter gh f = gh{gh_formatter = f}
    getFormatter gh = gh_formatter gh
    emit gh (prio, msg) _ = sendLog (gh_graylog gh) (createGelf gh prio msg)
    close gh = closeGraylog $ gh_graylog gh