{-# LANGUAGE TupleSections #-}

module Sisku.Server (app, getAllHovercrafts, toEntries) where

import Control.Lens (view, (^.))
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Network.Wai.Middleware.Rewrite (rewriteRoot)
import Relude
import Servant (Application, Get, JSON, Raw, Server, serve, serveDirectoryWebApp, type (:<|>) ((:<|>)), type (:>))
import Servant.API (QueryParam, QueryParams)
import Sisku.Config (HasProjectId (projectId))
import Sisku.Hovercraft
import qualified Sisku.Search as Search
import System.FilePath (takeBaseName, (</>))
import UnliftIO.Directory (XdgDirectory (XdgData), getXdgDirectory, listDirectory)

type API =
  "hovercraft" :> Get '[JSON] (Map Text Hovercraft)
    :<|> "search" :> QueryParam "placeholder" Text :> QueryParams "projectIds" Text :> QueryParam "q" Text :> Get '[JSON] (Search Entry)
    :<|> Raw

data Result a = Result
  { hit :: a,
    score :: Double
  }
  deriving stock (Generic)

instance ToJSON a => ToJSON (Result a)

data Search a = Search
  { query :: Text,
    results :: [Result a]
  }
  deriving stock (Generic)

instance ToJSON a => ToJSON (Search a)

api :: Proxy API
api = Proxy

server :: FilePath -> Map Text Hovercraft -> Server API
server staticFilePath hovercrafts =
  pure hovercrafts
    :<|> searchTokens (toEntries hovercrafts)
    :<|> serveDirectoryWebApp staticFilePath

toEntries :: Map Text Hovercraft -> [Entry]
toEntries hovercrafts = concatMap (\hovercraft -> concatMap (view entries) $ hovercraft ^. pages) $ Map.elems hovercrafts

searchTokens :: Applicative f => [Entry] -> Maybe Text -> [Text] -> Maybe Text -> f (Search Entry)
searchTokens _ _ _ Nothing = pure Search {query = "", results = []}
searchTokens es mplaceholder projectIds (Just x) = do
  let results =
        Search.search (fromMaybe "_" mplaceholder) es x
          & filter (\(e, _) -> (e ^. projectId) `elem` projectIds)
          & map (\(e, s) -> Result {hit = e, score = s})
  pure Search {query = x, results = results}

getAllHovercrafts :: MonadIO m => m (Map Text Hovercraft)
getAllHovercrafts = do
  dataDir <- getXdgDirectory XdgData "sisku/hovercraft"
  files <- listDirectory dataDir
  hovercrafts <- catMaybes <$> traverse (loadHovercraft dataDir) files
  pure $ Map.fromList hovercrafts
  where
    loadHovercraft dir file = do
      contents <- liftIO $ Aeson.decodeFileStrict (dir </> file)
      pure $ fmap (toText $ takeBaseName file,) contents

app :: FilePath -> Map Text Hovercraft -> Application
app staticFilePath hovercrafts =
  rewriteRoot "index.html" $ serve api (server staticFilePath hovercrafts)