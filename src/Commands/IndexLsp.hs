module Commands.IndexLsp (cmd, Options (..), parser) where

import Config
import Hovercraft (writeHovercraft)
import Lsp (buildHovercraft, generateBuildEnv)
import Options.Applicative
import Relude

data Options = Options
  { entryFilePath :: FilePath,
    configFilePath :: FilePath,
    outputFilePath :: Maybe FilePath
  }

cmd :: Options -> IO ()
cmd Options {..} = do
  config <- loadConfig configFilePath
  buildEnv <- generateBuildEnv config entryFilePath
  hovercraft <- buildHovercraft buildEnv
  writeHovercraft config outputFilePath hovercraft

opts :: Parser Options
opts =
  Options
    <$> strArgument (metavar "<entry file>")
    <*> strOption (short 'c' <> long "config" <> metavar "<config>" <> help "Path to the config file" <> value "sisku_config.json")
    <*> optional (strOption (short 'o' <> long "output" <> metavar "<output>" <> help "Path to the output file"))

parser :: Mod CommandFields (IO ())
parser = command "index-lsp" (info (cmd <$> opts) (progDesc "Make a index via LSP."))
