module Commands.NewIndexLsp (parser) where

import qualified Data.Aeson as Aeson
import Lsp (LspSettings, buildHovercraft, generateBuildEnv)
import Options.Applicative
import Relude

data NewIndexLspOptions = NewIndexLspOptions
  { nilEntryFilePath :: FilePath,
    nilLspSettingsFilePath :: FilePath,
    nilHovercraftFilePath :: FilePath
  }

newIndexLspCommand :: NewIndexLspOptions -> IO ()
newIndexLspCommand NewIndexLspOptions {..} = do
  lspSettings <- loadLspSettingsFromFile nilLspSettingsFilePath
  buildEnv <- generateBuildEnv lspSettings nilEntryFilePath
  hovercrafts <- buildHovercraft buildEnv
  Aeson.encodeFile nilHovercraftFilePath hovercrafts
  where
    loadLspSettingsFromFile :: FilePath -> IO LspSettings
    loadLspSettingsFromFile filePath = do
      contents <- readFileLBS filePath
      case Aeson.eitherDecode contents of
        Left err -> error (toText err)
        Right config -> pure config

newIndexLspOpts :: Parser NewIndexLspOptions
newIndexLspOpts =
  NewIndexLspOptions
    <$> strArgument (metavar "<entry file>")
    <*> strOption (short 's' <> long "settings" <> metavar "<lsp settings>" <> help "Path to the LSP settings file" <> value "lsp-settings.json")
    <*> strOption (short 'o' <> long "output" <> metavar "<file>" <> help "Write output to <file>" <> value "hovercraft.json")

parser :: Mod CommandFields (IO ())
parser = command "new-index-lsp" (info (newIndexLspCommand <$> newIndexLspOpts) (progDesc "Make a index via LSP."))
