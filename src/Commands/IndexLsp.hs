module Commands.IndexLsp (parser) where

import qualified Data.Aeson as Aeson
import Lsp (LspSettings, buildHovercraft, generateBuildEnv)
import Options.Applicative
import Relude

data Options = Options
  { entryFilePath :: FilePath,
    lspSettingsFilePath :: FilePath,
    hovercraftFilePath :: FilePath
  }

cmd :: Options -> IO ()
cmd Options {..} = do
  lspSettings <- loadLspSettingsFromFile lspSettingsFilePath
  buildEnv <- generateBuildEnv lspSettings entryFilePath
  hovercrafts <- buildHovercraft buildEnv
  Aeson.encodeFile hovercraftFilePath hovercrafts
  where
    loadLspSettingsFromFile :: FilePath -> IO LspSettings
    loadLspSettingsFromFile filePath = do
      contents <- readFileLBS filePath
      case Aeson.eitherDecode contents of
        Left err -> error (toText err)
        Right config -> pure config

opts :: Parser Options
opts =
  Options
      <$> strArgument (metavar "<entry file>")
    <*> strOption (short 's' <> long "settings" <> metavar "<lsp settings>" <> help "Path to the LSP settings file" <> value "lsp-settings.json")
    <*> strOption (short 'o' <> long "output" <> metavar "<file>" <> help "Write output to <file>" <> value "hovercraft.json")

parser :: Mod CommandFields (IO ())
parser = command "index-lsp" (info (cmd <$> opts) (progDesc "Make a index via LSP."))
