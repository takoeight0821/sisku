{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Parser for gen-elastic-index command
module Sisku.Commands.GenElasticIndex
  ( parser,
  )
where

import Control.Lens (ifor_)
import qualified Data.Aeson as Aeson
import Options.Applicative
import Relude
import Sisku.Hovercraft
import System.IO (hPutStrLn)

data Options = Options
  { hovercraftFilePath :: FilePath,
    indexFilePath :: FilePath
  }

cmd :: Options -> IO ()
cmd Options {..} = do
  hovercrafts :: Maybe [Hovercraft] <- Aeson.decodeFileStrict hovercraftFilePath
  case hovercrafts of
    Just hovercrafts ->
      withFile indexFilePath WriteMode $ \handle ->
        ifor_ hovercrafts \i hover -> do
          hPutStrLn handle $ "{ \"index\": {\"_index\": \"hovercraft\", \"_id\": \"" <> show i <> "\" } }"
          hPutStrLn handle $ decodeUtf8 $ Aeson.encode hover
    Nothing -> error "Fail to load the hovercraft file."

opts :: Parser Options
opts =
  Options
    <$> strOption (short 'i' <> long "input" <> metavar "<file>" <> help "Hovercraft index file" <> value "hovercraft.json")
    <*> strOption (short 'o' <> long "output" <> metavar "<file>" <> help "Write output to <file>" <> value "index.jsonl")

parser :: Mod CommandFields (IO ())
parser = command "gen-elastic-index" (info (cmd <$> opts) (progDesc "Generate the JSONL file for Elasticsearch"))
