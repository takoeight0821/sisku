module Sisku.Commands.Search (parser) where

import Options.Applicative
import Relude
import Sisku.Search

data Options = Options
  { query :: Text,
    elasticSearchFqdn :: Text,
    elasticSearchPort :: Int
  }

cmd :: Options -> IO ()
cmd Options {..} = search elasticSearchFqdn elasticSearchPort query

opts :: Parser Options
opts =
  Options
    <$> strArgument (metavar "SEARCH_QUERY")
    <*> strOption (long "elasticsearch-fqdn" <> value "localhost" <> showDefault <> help "Elasticsearch FQDN")
    <*> option auto (long "elasticsearch-port" <> value 9200 <> showDefault <> help "Elasticsearch port")

parser :: Mod CommandFields (IO ())
parser = command "search" $ info (cmd <$> opts) (progDesc "Sisku CLI for searching")