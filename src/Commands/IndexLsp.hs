module Commands.IndexLsp (parser) where

import Relude
import qualified Data.Aeson as Aeson
import Lsp
import Options.Applicative

data IndexLspOptions = IndexLspOptions
  { lspConfigFilePath :: FilePath,
    lspHovercraftFilePath :: FilePath
  }

indexLspCommand :: IndexLspOptions -> IO ()
indexLspCommand IndexLspOptions {..} = do
  lspConfig <- loadBuildEnvFromFile lspConfigFilePath
  hovercrafts <- buildHovercraft lspConfig
  Aeson.encodeFile lspHovercraftFilePath hovercrafts
  where
    loadBuildEnvFromFile :: FilePath -> IO BuildEnv
    loadBuildEnvFromFile filePath = do
      contents <- readFileLBS filePath
      case Aeson.eitherDecode contents of
        Left err -> error (toText err)
        Right config -> pure config

indexLspOpts :: Parser IndexLspOptions
indexLspOpts =
  IndexLspOptions
    <$> strOption (short 'c' <> long "config" <> metavar "<lsp config>" <> help "Path to the LSP config file" <> value "lsp-config.json")
    <*> strOption (short 'o' <> long "output" <> metavar "<file>" <> help "Write output to <file>" <> value "hovercraft.json")

parser :: Mod CommandFields (IO ())
parser = command "index-lsp" (info (indexLspCommand <$> indexLspOpts) (progDesc "Make a index via LSP."))
