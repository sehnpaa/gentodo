module Options (parseOptions, handleOptions) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM_)
import Data.Semigroup ((<>))
import Options.Applicative

import Types

version :: Parser (a -> a)
version = infoOption "v0.0.1" ( long "version" <> help "Show version")

opts :: ParserInfo Options
opts = info (helper <*> version <*> optionParser)
    ( fullDesc
    <> progDesc "Print a greeting for TARGET"
    <> header "gentodo - generate a todo list" )

optionParser :: Parser Options
optionParser = Options <$> commandParser

commandParser :: Parser Command
commandParser = pure Version

parseOptions :: [String] -> ParserResult Options
parseOptions = execParserPure defaultPrefs opts

handleOptions :: ParserResult a -> IO a
handleOptions = handleParseResult