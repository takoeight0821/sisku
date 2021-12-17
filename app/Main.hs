{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Control.Lens (ifor_)
import qualified Data.Aeson as Aeson
import Options.Applicative
import Relude hiding (id)
import Sisku
import System.IO (hPutStrLn)

opts :: Parser (IO ())
opts =
  hsubparser $
    command "index-lsif" (info (indexLsifCommand <$> indexLsifOpts) (progDesc "Make a index via LSIF."))
      <> command "index-lsp" (info (indexLspCommand <$> indexLspOpts) (progDesc "Make a index via LSP."))
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
