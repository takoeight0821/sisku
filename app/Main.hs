module Main where

import Control.Lens ((^?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key)
import Data.List (isInfixOf)
import Lib
import Options.Applicative
import Relude
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)
import Network.Wai.Middleware.Cors (simpleCors)

data SiskuOption = SiskuOption
  { filePath :: FilePath,
    query :: String,
    isServerMode :: Bool
  }

siskuOption :: Parser SiskuOption
siskuOption =
  SiskuOption
    <$> strArgument (metavar "INDEX_FILE")
    <*> strOption (metavar "QUERY" <> long "query" <> value "")
    <*> switch (long "server")

main :: IO ()
main = do
  opt <- execParser opts
  index <- loadFile (filePath opt)

  if isServerMode opt
     then run 8080 (simpleCors $ serve (Proxy :: Proxy SearchApi) (searchServer index))
     else putLBS $ Aeson.encode $ Aeson.Array $ fromList $ map Aeson.toJSON (search index (filterByQuery $ query opt))
  where
    opts =
      info
        (siskuOption <**> helper)
        (fullDesc <> progDesc "Sisku" <> header "sisku")
    filterByQuery [] _ = True
    filterByQuery q SearchResult {..} =
      case hover ^? key "result" . key "contents" . key "value" of
        Just (Aeson.String contents) -> q `isInfixOf` toString contents
        _ -> False

