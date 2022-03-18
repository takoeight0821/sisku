module Sisku.Commands.IndexLsp (cmd, Options (..), parser) where

import Options.Applicative
import Relude
import Sisku.App
import Sisku.Config
import Sisku.Hovercraft (writeHovercraft)
import Sisku.Indexer
import Sisku.Indexer.Exhaustive
import Sisku.Indexer.ExtractCodeBlock

data Options = Options
  { configFilePath :: FilePath,
    outputFilePath :: Maybe FilePath
  }

cmd :: Options -> IO ()
cmd Options {..} = do
  config <- loadConfig configFilePath
  runSiskuApp config do
    let ExhaustiveIndexer indexer = build (defaultLanguageClient & extractCodeBlock)
    hovercraft <- indexer
    writeHovercraft outputFilePath hovercraft

opts :: Parser Options
opts =
  Options
    <$> strOption (short 'c' <> long "config" <> metavar "<config>" <> help "Path to the config file" <> value "sisku_config.json")
    <*> optional (strOption (short 'o' <> long "output" <> metavar "<output>" <> help "Path to the output file"))

parser :: Mod CommandFields (IO ())
parser = command "index-lsp" (info (cmd <$> opts) (progDesc "Make a index via LSP."))
