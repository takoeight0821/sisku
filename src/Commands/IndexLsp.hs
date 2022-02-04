module Commands.IndexLsp (parser) where

import qualified Data.Aeson as Aeson
import Lsp (LspSettings, buildHovercraft, generateBuildEnv, BuildEnv (buildEnvRootPath))
import Options.Applicative
import Relude
import System.FilePath (takeBaseName, dropTrailingPathSeparator)
import Hovercraft (writeHovercraft)

data Options = Options
  { entryFilePath :: FilePath,
    lspSettingsFilePath :: FilePath
  }

cmd :: Options -> IO ()
cmd Options {..} = do
  lspSettings <- loadLspSettingsFromFile lspSettingsFilePath
  buildEnv <- generateBuildEnv lspSettings entryFilePath
  hovercraft <- buildHovercraft buildEnv
  let projectName = takeBaseName $ dropTrailingPathSeparator $ buildEnvRootPath buildEnv
  writeHovercraft projectName hovercraft 
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

parser :: Mod CommandFields (IO ())
parser = command "index-lsp" (info (cmd <$> opts) (progDesc "Make a index via LSP."))
