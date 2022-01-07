module Commands.IndexLsif (parser) where

import Relude
import qualified Data.Aeson as Aeson
import Options.Applicative
import Lsif

data IndexLsifOptions = IndexLsifOptions
  { lsifFilePath :: FilePath,
    lsifHovercraftFilePath :: FilePath
  }

indexLsifCommand :: IndexLsifOptions -> IO ()
indexLsifCommand IndexLsifOptions {lsifFilePath, lsifHovercraftFilePath} = do
  hovercrafts <- indexToHovercraft <$> loadLsifFromFile lsifFilePath
  Aeson.encodeFile lsifHovercraftFilePath hovercrafts

indexLsifOpts :: Parser IndexLsifOptions
indexLsifOpts =
  IndexLsifOptions
    <$> strArgument (metavar "<index file>")
    <*> strOption (short 'o' <> long "output" <> metavar "<file>" <> help "Write output to <file>" <> value "hovercraft.json")

parser :: Mod CommandFields (IO ())
parser = command "index-lsif" (info (indexLsifCommand <$> indexLsifOpts) (progDesc "Make a index via LSIF."))
