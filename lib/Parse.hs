module Parse where

import Control.Monad (void)
import Data.Char (isDigit)
import qualified Data.Text as T
import Data.Time.Calendar (diffDays, fromGregorian)
import Safe (maximumMay)
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as PT

import Types

parseErrorToText :: P.ParseError -> T.Text
parseErrorToText = T.pack . show

process :: Date -> T.Text -> T.Text -> Either P.ParseError [Output]
process currentDate logContent ambitionsContent = do
    logs <- parseLogEntries logContent
    ambitions <- parse ambitionsContent
    return $ getOutputs currentDate logs ambitions

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
    Ambition (Frequency (read [n]) Day) <$> parseDescription

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
    LogEntry date <$> parseDescription

parseDate :: PT.Parser Date
parseDate = do
    year <- parseNumber
    void $ P.char '-'
    month <- parseNumber
    void $ P.char '-'
    fromGregorian (read year) (read month) . read <$> parseNumber


parseNumber :: PT.Parser String
parseNumber = P.many P.digit

getOutputs :: Date -> [LogEntry] -> [Ambition] -> [Output]
getOutputs currentDate logEntries =
    map description . filter (relevantAmb currentDate logEntries)

relevantAmb :: Date -> [LogEntry] -> Ambition -> Bool
relevantAmb date logEntries a =
    case getLatestLogEntry logEntries a of
        Nothing -> True
        (Just dateFoundInLog) ->
            diffDays date dateFoundInLog >= num (frequency a)

getLatestLogEntry :: [LogEntry] -> Ambition -> Maybe Date
getLatestLogEntry logEntries a =
    getLatestDate $ filter (hasSameDescription a) logEntries

hasSameDescription :: Ambition -> LogEntry -> Bool
hasSameDescription a (LogEntry _ logDesc) = logDesc == description a

getLatestDate :: [LogEntry] -> Maybe Date
getLatestDate logEntries = maximumMay $ map logDate logEntries

