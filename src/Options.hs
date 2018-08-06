module Options (execParser, opts) where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (replicateM_)

import Types

version :: Parser Options
version = fmap (const $ Options Version) (infoOption "v0.0.1" ( long "version" <> help "Show version"))

opts :: ParserInfo Options
opts = info (helper <*> version)
    ( fullDesc
    <> progDesc "Print a greeting for TARGET"
    <> header "gentodo - generate a todo list" )