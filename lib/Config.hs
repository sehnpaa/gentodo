{-# LANGUAGE DeriveGeneric #-}

module Config where

import Dhall

data Config = Config
  { ambitionsPath :: String
  , logPath :: String
  , todoPath :: String } deriving (Generic, Show)

instance Interpret Config

loadConfig :: Text -> IO Config
loadConfig = input auto
