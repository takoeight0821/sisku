{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Control.Lens (ifor_)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.String as String
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (simpleCors)
import Options.Applicative
import Relude hiding (id)
import qualified Relude.Unsafe as Unsafe
import Servant.Server (serve)
import Sisku
import System.IO (hPutStrLn)

opts :: Parser (IO ())
opts =
  hsubparser $
    command "index-lsif" (info (indexLsifCommand <$> indexLsifOpts) (progDesc "Make a index via LSIF."))
      <> command "index-lsp" (info (indexLspCommand <$> indexLspOpts) (progDesc "Make a index via LSP."))
      <> command "server" (info (serverCommand <$> serverOpts) (progDesc "Start the Sisku server."))
      <> command "search" (info (searchCommand <$> searchOpts) (progDesc "CLI interface"))
      <> command "gen-elastic-index" (info (genElasticIndexCommand <$> genElasticIndexOpts) (progDesc "Generate the JSONL file for Elasticsearch"))

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

data IndexLspOptions = IndexLspOptions
  { lspConfigFilePath :: FilePath,
    lspHovercraftFilePath :: FilePath
  }

data LspConfig = LspConfig
  { lspConfigCommand :: FilePath,
    lspConfigRootPath :: FilePath,
    lspConfigExtension :: String
  }

instance ToJSON LspConfig where
  toJSON LspConfig {lspConfigCommand, lspConfigRootPath, lspConfigExtension} =
    Aeson.object
      [ "command" Aeson..= lspConfigCommand,
        "rootPath" Aeson..= lspConfigRootPath,
        "extension" Aeson..= lspConfigExtension
      ]

instance FromJSON LspConfig where
  parseJSON = Aeson.withObject "LspConfig" $ \o ->
    LspConfig
      <$> o Aeson..: "command"
      <*> o Aeson..: "rootPath"
      <*> o Aeson..: "extension"

indexLspCommand :: IndexLspOptions -> IO ()
indexLspCommand IndexLspOptions {..} = do
  LspConfig {..} <- loadLspConfigFromFile lspConfigFilePath
  let command = String.words lspConfigCommand
  hovercrafts <- buildHovercraft lspConfigRootPath lspConfigExtension (Unsafe.head command) (Unsafe.tail command)
  Aeson.encodeFile lspHovercraftFilePath hovercrafts
  where
    loadLspConfigFromFile :: FilePath -> IO LspConfig
    loadLspConfigFromFile filePath = do
      contents <- readFileLBS filePath
      case Aeson.eitherDecode contents of
        Left err -> error (toText err)
        Right config -> pure config

indexLspOpts :: Parser IndexLspOptions
indexLspOpts =
  IndexLspOptions
    <$> strOption (short 'c' <> long "config" <> metavar "<lsp config>" <> help "Path to the LSP config file" <> value "lsp-config.json")
    <*> strOption (short 'o' <> long "output" <> metavar "<file>" <> help "Write output to <file>" <> value "hovercraft.json")

data ServerOptions = ServerOptions
  { serverHovercraftFilePath :: FilePath,
    serverPort :: Int
  }

serverCommand :: ServerOptions -> IO ()
serverCommand ServerOptions {..} = do
  hovercrafts <- Aeson.decodeFileStrict serverHovercraftFilePath
  case hovercrafts of
    Just hovercrafts -> withStdoutLogger $ \aplogger -> do
      let settings = setPort serverPort $ setLogger aplogger defaultSettings
      putStrLn $ "Listing on port " <> show serverPort
      runSettings settings (simpleCors $ serve (Proxy :: Proxy SearchApi) (searchServer hovercrafts))
    Nothing -> error "Fail to load the hovercraft file."

serverOpts :: Parser ServerOptions
serverOpts =
  ServerOptions
    <$> strOption (short 'i' <> long "input" <> metavar "<file>" <> help "Hovercraft index file" <> value "hovercraft.json")
    <*> option auto (short 'p' <> long "port" <> metavar "<port>" <> help "Port (default: 8081)" <> value 8081)

data SearchOptions = SearchOptions
  { searchHovercraftFilePath :: FilePath,
    searchQuery :: Text
  }

searchCommand :: SearchOptions -> IO ()
searchCommand SearchOptions {..} = do
  hovercrafts <- Aeson.decodeFileStrict searchHovercraftFilePath
  case hovercrafts of
    Just hovercrafts ->
      putLBS $ Aeson.encode $ Aeson.Array $ fromList $ map Aeson.toJSON $ filter (filterByQuery searchQuery) hovercrafts
    Nothing -> error "Fail to load the hovercraft file."

searchOpts :: Parser SearchOptions
searchOpts =
  SearchOptions
    <$> strOption (short 'i' <> long "input" <> metavar "<file>" <> help "Hovercraft index file" <> value "hovercraft.json")
    <*> strArgument (metavar "<query>" <> help "Search by <query>")

data GenElasticIndexOptions = GenElasticIndexOptions
  { elasticHovercraftFilePath :: FilePath,
    elasticIndexFilePath :: FilePath
  }

genElasticIndexCommand :: GenElasticIndexOptions -> IO ()
genElasticIndexCommand GenElasticIndexOptions {..} = do
  hovercrafts :: Maybe [Hovercraft] <- Aeson.decodeFileStrict elasticHovercraftFilePath
  case hovercrafts of
    Just hovercrafts ->
      withFile elasticIndexFilePath WriteMode $ \handle ->
        ifor_ hovercrafts \i hover -> do
          hPutStrLn handle $ "{ \"index\": {\"_index\": \"hovercraft\", \"_id\": \"" <> show i <> "\" } }"
          hPutStrLn handle $ decodeUtf8 $ Aeson.encode hover
    Nothing -> error "Fail to load the hovercraft file."

genElasticIndexOpts :: Parser GenElasticIndexOptions
genElasticIndexOpts =
  GenElasticIndexOptions
    <$> strOption (short 'i' <> long "input" <> metavar "<file>" <> help "Hovercraft index file" <> value "hovercraft.json")
    <*> strOption (short 'o' <> long "output" <> metavar "<file>" <> help "Write output to <file>" <> value "index.jsonl")

main :: IO ()
main = join $ execParser (info (opts <**> helper) (fullDesc <> header "Sisku - Polyglot API Search Engine"))
