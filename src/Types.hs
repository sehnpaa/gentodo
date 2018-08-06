module Types where

import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal

data DWMY = Day | Week | Month | Year deriving Show

data Frequency = Frequency
  { num :: Integer
  , unit :: DWMY } deriving Show

data Ambition = Ambition
  { frequency :: Frequency
  , description :: T.Text } deriving Show

data LogEntry = LogEntry
  { logDate :: Date
  , logDescription :: T.Text } deriving Show

type Output = T.Text

type Todo = T.Text

type Date = Cal.Day

data Options = Options
  { command :: Command }

data Command = Version