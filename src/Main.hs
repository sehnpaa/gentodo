{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime, utctDay)
import Options.Applicative
import System.Environment (getArgs)
import Text.Read (read)

import Config (ambitionsFilename, logFilename, todoFilename)
import Options (handleOptions, parseOptions)
import Parse (parseErrorToText, process)
import Types

------ *.txt-files

-- ambitions.txt 
---- Where the user writes his/her routine/ambitions

-- log.txt
---- A list of events so the application can keep track of whats 
---- already been appended to todo.txt

-- todo.txt
---- Entries can be modified manually by the user (via a text editor)
---- New entries will be appended to this file by the application

main :: IO ()
main = do
    args <- getArgs
    options <- handleOptions $ parseOptions args
    runWithOptions options

-- Get content from the ambition file and log file together
-- with the current date. Apply process to this data and 
-- either print the error or write to both the log file
-- and the todo file.

runWithOptions :: Options -> IO ()
runWithOptions _ = do
    ambitionsContent <- TIO.readFile ambitionsFilename
    logContent <- TIO.readFile logFilename
    currentDate <- getDate

    either (print . parseErrorToText) (\res -> do
        writeToLogFile $ map formatLogEntry $ getLogEntries currentDate res
        writeToTodoFile $ getTodos res)
        $ process currentDate logContent ambitionsContent

    return ()

writeToLogFile :: [T.Text] -> IO ()
writeToLogFile =
    mapM_ (\line -> TIO.appendFile logFilename line >> printToCLI (getWriteToLogMessage line))

printToCLI :: T.Text -> IO ()
printToCLI = TIO.putStrLn

getWriteToLogMessage :: T.Text -> T.Text
-- showt will include newline character
getWriteToLogMessage s = T.concat ["Writing ", s, " to log file"]

writeToTodoFile :: [T.Text] -> IO ()
writeToTodoFile =
    mapM_ (\line -> TIO.appendFile todoFilename line >> printToCLI (getWriteToTodoMessage line))

getWriteToTodoMessage :: T.Text -> T.Text
getWriteToTodoMessage s = T.concat ["Writing ", s, " to todo file"]

formatLogEntry :: LogEntry -> T.Text
formatLogEntry (LogEntry date desc) =
    T.concat [showDate date, ";", desc, "\n"]

getDate :: IO Date
getDate = fmap utctDay getCurrentTime

getLogEntries :: Date -> [Output] -> [LogEntry]
getLogEntries currentDate = map (toLog currentDate)

toLog :: Date -> Output -> LogEntry
toLog = LogEntry

showDate :: Date -> T.Text
showDate day = T.pack $ show day

getTodos :: [Output] -> [Todo]
getTodos = map (T.append "\n")