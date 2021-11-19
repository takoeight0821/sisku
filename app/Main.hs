{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import qualified Data.Aeson as Aeson
import LspClient (runLspClient)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Options.Applicative
import Relude hiding (id)
import Servant.Server (serve)
import Sisku

data SiskuOption = SiskuOption
  { filePath :: FilePath,
    query :: String,
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
  if
      | isServerMode opt -> do
        index <- loadFile (filePath opt)
        run 8080 (simpleCors $ serve (Proxy :: Proxy SearchApi) (searchServer index))
      | isLspClientMode opt -> runLspClient "." "src/LspClient.hs" "haskell-language-server-wrapper" ["--lsp"]
      | otherwise -> do
        index <- loadFile (filePath opt)
        putLBS $ Aeson.encode $ Aeson.Array $ fromList $ map Aeson.toJSON (search index (filterByQuery $ query opt))
  where
    opts =
      info
        (siskuOption <**> helper)
        (fullDesc <> progDesc "Sisku" <> header "sisku")
