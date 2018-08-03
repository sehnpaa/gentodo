{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (Day, diffDays, fromGregorian)
import Safe (maximumMay)
import System.IO
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as PT
import Text.Read (read)
import TextShow (showt, printT)

------ *.txt-files

-- ambitions.txt 
---- Where the user writes his/her routine/ambitions

-- log.txt
---- A list of events so the application can keep track of whats 
---- already been appended to todo.txt

-- todo.txt
---- Entries can be modified manually by the user (via a text editor)
---- New entries will be appended to this file by the application

ambitionsFilename :: String
ambitionsFilename = "ambitions.txt"

logFilename :: String
logFilename = "log.txt"

todoFilename :: String
todoFilename = "todo.txt"

-- Get content from the ambition file and log file together
-- with the current date. Apply process to this data and 
-- either print the error or write to both the log file
-- and the todo file.

main :: IO ()
main = do
    ambitionsContent <- TIO.readFile ambitionsFilename
    logContent <- TIO.readFile logFilename
    currentDate <- getDate

    either (printT . parseErrorToText) (\res -> do
        writeToLogFile $ map formatLogEntry $ getLogEntries currentDate res
        writeToTodoFile $ getTodos res)
        $ process currentDate logContent ambitionsContent
 
    return ()

parseErrorToText :: P.ParseError -> T.Text
parseErrorToText = T.pack . show

process :: Day -> T.Text -> T.Text -> Either P.ParseError [Output]
process currentDate logContent ambitionsContent = do
    logs <- parseLogEntries logContent
    ambitions <- parse ambitionsContent
    return $ getOutputs currentDate logs ambitions

data DWMY = Day | Week | Month | Year deriving Show

data Frequency = Frequency
  { num :: Integer
  , unit :: DWMY } deriving Show

data Ambition = Ambition
  { frequency :: Frequency
  , description :: T.Text } deriving Show

data LogEntry = LogEntry
  { logDate :: Day
  , logDescription :: T.Text } deriving Show

type Output = T.Text

type Todo = T.Text

writeToLogFile :: [T.Text] -> IO ()
writeToLogFile =
    mapM_ (\line -> TIO.appendFile logFilename line >> printToCLI (getWriteToLogMessage line))

printToCLI :: T.Text -> IO ()
printToCLI = TIO.putStrLn

getWriteToLogMessage :: T.Text -> T.Text
-- showt will include newline character
getWriteToLogMessage s = T.concat ["Writing ", showt s, " to log file"]

writeToTodoFile :: [T.Text] -> IO ()
writeToTodoFile =
    mapM_ (\line -> TIO.appendFile todoFilename line >> printToCLI (getWriteToTodoMessage line))

getWriteToTodoMessage :: T.Text -> T.Text
getWriteToTodoMessage s = T.concat ["Writing ", showt s, " to todo file"]

formatLogEntry :: LogEntry -> T.Text
formatLogEntry (LogEntry date desc) =
    T.concat [showDate date, ";", desc, "\n"]

parse :: T.Text-> Either P.ParseError [Ambition]
parse = P.parse parseAmbitions ""

parseAmbitions :: PT.Parser [Ambition]
parseAmbitions = P.endBy parseAmbition eol

eol :: PT.Parser String
eol = P.string "\n"

parseAmbition :: PT.Parser Ambition
parseAmbition = do
    n <- P.satisfy isDigit
    void $ P.char ';'
    desc <- parseDescription
    return $ Ambition (Frequency (read [n]) Day) desc

parseDescription :: PT.Parser T.Text
parseDescription = do
    desc <- P.many $ P.noneOf "\n"
    return (T.pack desc)

parseLogEntries :: T.Text -> Either P.ParseError [LogEntry]
parseLogEntries = P.parse parseLogEntries2 ""

parseLogEntries2 :: PT.Parser [LogEntry]
parseLogEntries2 = P.endBy parseLogEntry eol

parseLogEntry :: PT.Parser LogEntry
parseLogEntry = do
    date <- parseDate
    void $ P.char ';'
    desc <- parseDescription
    return $ LogEntry date desc

parseDate :: PT.Parser Day
parseDate = do
    year <- parseNumber
    void $ P.char '-'
    month <- parseNumber
    void $ P.char '-'
    day <- parseNumber
    return $ fromGregorian (read year) (read month) (read day)

parseNumber :: PT.Parser String
parseNumber = P.many P.digit

getDate :: IO Day
getDate = fmap utctDay getCurrentTime

getOutputs :: Day -> [LogEntry] -> [Ambition] -> [Output]
getOutputs currentDate logEntries =
    map description . filter (relevantAmb currentDate logEntries)

relevantAmb :: Day -> [LogEntry] -> Ambition -> Bool
relevantAmb date logEntries a =
    case getLatestLogEntry logEntries a of
        Nothing -> True
        (Just dateFoundInLog) ->
            diffDays date dateFoundInLog >= num (frequency a)

getLatestLogEntry :: [LogEntry] -> Ambition -> Maybe Day
getLatestLogEntry logEntries a =
    getLatestDate $ filter (hasSameDescription a) logEntries

hasSameDescription :: Ambition -> LogEntry -> Bool
hasSameDescription a (LogEntry _ logDesc) = logDesc == description a

getLatestDate :: [LogEntry] -> Maybe Day
getLatestDate logEntries = maximumMay $ map logDate logEntries

getLogEntries :: Day -> [Output] -> [LogEntry]
getLogEntries currentDate = map (toLog currentDate)

toLog :: Day -> Output -> LogEntry
toLog = LogEntry

showDate :: Day -> T.Text
showDate day = T.pack $ show day

getTodos :: [Output] -> [Todo]
getTodos = map (T.append "\n")