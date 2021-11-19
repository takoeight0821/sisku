{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import qualified Data.Aeson as Aeson
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Options.Applicative
import Relude hiding (id)
import Servant.Server (serve)
import Sisku

data SiskuOption = SiskuOption
  { filePath :: FilePath,
    query :: Text,
    isServerMode :: Bool,
    isLspClientMode :: Bool
  }

siskuOption :: Parser SiskuOption
siskuOption =
  SiskuOption
    <$> strOption (metavar "INDEX_FILE" <> long "index" <> value "")
    <*> strOption (metavar "QUERY" <> long "query" <> value "")
    <*> switch (long "server")
    <*> switch (long "lsp-client")

main :: IO ()
main = do
  opt <- execParser opts
  hovercrafts <-
    if isLspClientMode opt
      then buildHovercraft "src" "hs" "haskell-language-server-wrapper" ["--lsp"]
      else indexToHovercraft <$> loadLsifFromFile (filePath opt)
  if isServerMode opt
    then run 8081 (simpleCors $ serve (Proxy :: Proxy SearchApi) (searchServer hovercrafts))
    else putLBS $ Aeson.encode $ Aeson.Array $ fromList $ map Aeson.toJSON (filter (filterByQuery $ query opt) hovercrafts)
  where
    opts =
      info
        (siskuOption <**> helper)
        (fullDesc <> progDesc "Sisku" <> header "sisku")
