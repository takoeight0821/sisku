module Server (app) where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Hovercraft
import Relude
import Servant (Application, Get, JSON, Raw, Server, serve, serveDirectoryWebApp, type (:<|>) ((:<|>)), type (:>))
import System.FilePath (takeBaseName, (</>))
import UnliftIO.Directory (XdgDirectory (XdgData), getXdgDirectory, listDirectory)

type API =
  "hovercraft" :> Get '[JSON] (Map Text Hovercraft)
    :<|> Raw

api :: Proxy API
api = Proxy

server :: FilePath -> Server API
server staticFilePath =
  getAllHovercrafts
    :<|> serveDirectoryWebApp staticFilePath

getAllHovercrafts :: MonadIO m => m (Map Text Hovercraft)
getAllHovercrafts = do
  dataDir <- getXdgDirectory XdgData "sisku/hovercraft"
  files <- listDirectory dataDir
  hovercrafts <- traverse (loadHovercraft dataDir) files
  pure $ Map.fromList hovercrafts
  where
    loadHovercraft dir file = do
      contents <- liftIO $ Aeson.decodeFileStrict (dir </> file)
      case contents of
        Nothing -> error $ "Could not decode file " <> toText file
        Just hovercraft -> pure (toText $ takeBaseName file, hovercraft)

app :: FilePath -> Application
app staticFilePath = serve api (server staticFilePath)