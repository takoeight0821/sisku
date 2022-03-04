module Sisku.Commands.IndexLsif (parser) where

import qualified Data.Aeson as Aeson
import Options.Applicative
import Relude
import Sisku.Lsif

data IndexLsifOptions = IndexLsifOptions
  { lsifFilePath :: FilePath,
    hovercraftFilePath :: FilePath
  }

indexLsifCommand :: IndexLsifOptions -> IO ()
indexLsifCommand IndexLsifOptions {lsifFilePath, hovercraftFilePath} = do
  hovercrafts <- indexToHovercraft <$> loadLsifFromFile lsifFilePath
  Aeson.encodeFile hovercraftFilePath hovercrafts

indexLsifOpts :: Parser IndexLsifOptions
indexLsifOpts =
  IndexLsifOptions
    <$> strArgument (metavar "<index file>")
    <*> strOption (short 'o' <> long "output" <> metavar "<file>" <> help "Write output to <file>" <> value "hovercraft.json")

parser :: Mod CommandFields (IO ())
parser = command "index-lsif" (info (indexLsifCommand <$> indexLsifOpts) (progDesc "Make a index via LSIF."))
