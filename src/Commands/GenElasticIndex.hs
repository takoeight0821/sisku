{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- | Parser for gen-elastic-index command
module Commands.GenElasticIndex (
    parser
) where

import Relude
import Hovercraft
import qualified Data.Aeson as Aeson
import Options.Applicative
import Control.Lens (ifor_)
import System.IO (hPutStrLn)

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

parser :: Mod CommandFields (IO ())
parser = command "gen-elastic-index" (info (genElasticIndexCommand <$> genElasticIndexOpts) (progDesc "Generate the JSONL file for Elasticsearch"))
