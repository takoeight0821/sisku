module Sisku.Server (app, getAllHovercrafts, toEntries) where

import Codec.Serialise (deserialise)
import Control.Lens (view, (^.))
import Data.Aeson (ToJSON)
import Data.ByteString.Lazy qualified as BSL
import Data.Map qualified as Map
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.Rewrite (rewriteRoot)
import Relude
import Servant (Application, Get, JSON, Raw, Server, serve, serveDirectoryWebApp, type (:<|>) ((:<|>)), type (:>))
import Servant.API (QueryParam, QueryParams)
import Sisku.Config (HasProjectId (projectId))
import Sisku.Hovercraft
import Sisku.Search (SearchResult (SearchResult))
import Sisku.Search qualified as Search
import System.FilePath (isExtensionOf, takeBaseName, (</>))
import UnliftIO.Directory (XdgDirectory (XdgData), getXdgDirectory, listDirectory)

type API =
  "hovercraft" :> Get '[JSON] (Map Text Hovercraft)
    :<|> "search" :> QueryParam "placeholder" Text :> QueryParams "projectIds" Text :> QueryParam "q" Text :> Get '[JSON] Search
    :<|> Raw

data Search = Search
  { query :: Text,
    results :: [SearchResult]
  }
  deriving stock (Generic)

instance ToJSON Search

api :: Proxy API
api = Proxy

server :: FilePath -> Map Text Hovercraft -> Server API
server staticFilePath hovercrafts =
  pure hovercrafts
    :<|> searchTokens (toEntries hovercrafts)
    :<|> serveDirectoryWebApp staticFilePath

toEntries :: Map Text Hovercraft -> [Entry]
toEntries hovercrafts = concatMap (\hovercraft -> concatMap (view entries) $ hovercraft ^. pages) $ Map.elems hovercrafts

searchTokens :: Applicative f => [Entry] -> Maybe Text -> [Text] -> Maybe Text -> f Search
searchTokens _ _ _ Nothing = pure Search {query = "", results = []}
searchTokens es mplaceholder projectIds (Just x) = do
  let results =
        Search.search (fromMaybe "_" mplaceholder) es x
          & filter (\SearchResult {hit = e} -> (e ^. projectId) `elem` projectIds)
  pure Search {query = x, results = results}

getAllHovercrafts :: MonadIO m => m (Map Text Hovercraft)
getAllHovercrafts = do
  dataDir <- getXdgDirectory XdgData "sisku/hovercraft"
  files <- filter (isExtensionOf "cbor") <$> listDirectory dataDir
  hovercrafts <- traverse (loadHovercraft dataDir) files
  pure $ Map.fromList hovercrafts
  where
    loadHovercraft dir file = do
      contents <- liftIO $ deserialise <$> BSL.readFile (dir </> file)
      pure (toText $ takeBaseName file, contents :: Hovercraft)

app :: FilePath -> Map Text Hovercraft -> Application
app staticFilePath hovercrafts =
  simpleCors $
    rewriteRoot "index.html" $
      serve api (server staticFilePath hovercrafts)