{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime, utctDay)
import System.Environment (getArgs)

import Config (Config, ambitionsPath, loadConfig, logPath, todoPath)
import Lib
  ( getFormatedEntries
  , getTodos
  , getWriteToLogMessage
  , getWriteToTodoMessage )
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

configPath :: T.Text
configPath = "./config.dhall"

runWithOptions :: Options -> IO ()
runWithOptions _ = do
    config <- loadConfig configPath
    ambitionsContent <- TIO.readFile $ ambitionsPath config
    logContent <- TIO.readFile $ logPath config
    currentDate <- getDate

    either (print . parseErrorToText) (writeToFiles currentDate config)
        (process currentDate logContent ambitionsContent)

    return ()

writeToFiles :: Date -> Config -> [Output] -> IO ()
writeToFiles date config res = do
        writeToLogFile (logPath config) $ getFormatedEntries date res
        writeToTodoFile (todoPath config) $ getTodos res

writeToLogFile :: String -> [T.Text] -> IO ()
writeToLogFile logPath' =
    mapM_ (\line -> TIO.appendFile logPath' line >> printToCLI (getWriteToLogMessage line))

printToCLI :: T.Text -> IO ()
printToCLI = TIO.putStrLn

writeToTodoFile :: String -> [T.Text] -> IO ()
writeToTodoFile todoPath' =
    mapM_ (\line -> TIO.appendFile todoPath' line >> printToCLI (getWriteToTodoMessage line))

getDate :: IO Date
getDate = fmap utctDay getCurrentTime
