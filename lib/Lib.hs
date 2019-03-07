{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.Text as T

import Types

getFormatedEntries :: Date -> [Output] -> [T.Text]
getFormatedEntries currentDate = map formatLogEntry . getLogEntries currentDate

getWriteToLogMessage :: T.Text -> T.Text
-- showt will include newline character
getWriteToLogMessage s = T.concat ["Writing ", s, " to log file"]

getWriteToTodoMessage :: T.Text -> T.Text
getWriteToTodoMessage s = T.concat ["Writing ", s, " to todo file"]

formatLogEntry :: LogEntry -> T.Text
formatLogEntry (LogEntry date desc) =
  T.concat [showDate date, ";", desc, "\n"]

getLogEntries :: Date -> [Output] -> [LogEntry]
getLogEntries currentDate = map (toLog currentDate)

toLog :: Date -> Output -> LogEntry
toLog = LogEntry

showDate :: Date -> T.Text
showDate = T.pack . show

getTodos :: [Output] -> [Todo]
getTodos = map (T.append "\n")
