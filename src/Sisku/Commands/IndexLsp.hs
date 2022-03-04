module Sisku.Commands.IndexLsp (cmd, Options (..), parser) where

import Options.Applicative
import Relude
import Sisku.App
import Sisku.Config
import Sisku.Hovercraft (writeHovercraft)
import Sisku.Indexer
import Sisku.Indexer.Common
import Sisku.Indexer.MiddlewareExample (exampleLanguageClient)

data Options = Options
  { entryFilePath :: FilePath,
    configFilePath :: FilePath,
    outputFilePath :: Maybe FilePath
  }

cmd :: Options -> IO ()
cmd Options {..} = do
  config <- loadConfig configFilePath
  runSiskuApp config do
    let CommonIndexer indexer = build exampleLanguageClient
    hovercraft <- indexer entryFilePath
    writeHovercraft outputFilePath hovercraft

opts :: Parser Options
opts =
  Options
    <$> strArgument (metavar "<entry file>")
    <*> strOption (short 'c' <> long "config" <> metavar "<config>" <> help "Path to the config file" <> value "sisku_config.json")
    <*> optional (strOption (short 'o' <> long "output" <> metavar "<output>" <> help "Path to the output file"))

parser :: Mod CommandFields (IO ())
parser = command "index-lsp" (info (cmd <$> opts) (progDesc "Make a index via LSP."))
