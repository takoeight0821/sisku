{-# LANGUAGE TupleSections #-}

module Sisku.Server (app, getAllHovercrafts, addDocuments, searchEngine) where

import Control.Lens (At (at), Ixed (ix), view, (^.), (^?!))
import Control.Lens.Fold ((^?))
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key, _String)
import Data.Aeson.Types (Value (Object))
import qualified Data.HashMap.Strict as HashMap
import Data.Ix (Ix)
import qualified Data.Map as Map
import Data.SearchEngine
import qualified Data.SearchEngine as SearchEngine
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Language.LSP.Types (HoverContents (HoverContents, HoverContentsMS))
import Language.LSP.Types.Lens (HasContents (contents), HasHover (hover), HasValue (value))
import NLP.Tokenize.Text (tokenize)
import Network.Wai.Middleware.Rewrite (rewriteRoot)
import Relude
import Servant (Application, Get, JSON, Raw, Server, serve, serveDirectoryWebApp, type (:<|>) ((:<|>)), type (:>))
import Servant.API (QueryParam)
import Sisku.Hovercraft
import System.FilePath (takeBaseName, (</>))
import UnliftIO.Directory (XdgDirectory (XdgData), getXdgDirectory, listDirectory)

type API =
  "hovercraft" :> Get '[JSON] (Map Text Hovercraft)
    :<|> "search" :> QueryParam "q" Text :> Get '[JSON] (Search Entry)
    :<|> Raw

data Search a = Search
  { query :: Text,
    results :: [a]
  }
  deriving stock (Generic)

instance ToJSON a => ToJSON (Search a)

api :: Proxy API
api = Proxy

server :: FilePath -> Map Text Hovercraft -> HashMap UUID Entry -> SearchEngine Doc UUID DocField NoFeatures -> Server API
server staticFilePath hovercrafts kvs engine =
  pure hovercrafts
    :<|> search kvs engine
    :<|> serveDirectoryWebApp staticFilePath

search :: Applicative f => HashMap UUID Entry -> SearchEngine Doc UUID DocField NoFeatures -> Maybe Text -> f (Search Entry)
search _ _ Nothing = pure Search {query = "", results = []}
search kvs engine (Just x) = pure Search {query = x, results = map (\k -> kvs ^?! ix k) (SearchEngine.query engine (tokenize x))}

data Doc = Doc
  { uuid :: UUID,
    entry :: Entry
  }

genDoc :: MonadIO m => Entry -> m Doc
genDoc entry = do
  uuid <- liftIO UUID.nextRandom
  pure Doc {..}

data DocField = FHoverContents | FSignature
  deriving stock (Eq, Ord, Ix, Bounded)

searchEngine :: SearchEngine Doc UUID DocField NoFeatures
searchEngine =
  initSearchEngine
    ( SearchConfig
        { documentKey = uuid,
          extractDocumentTerms = extractTokens,
          transformQueryTerm = const,
          documentFeatureValue = const noFeatures
        }
    )
    ( SearchRankParameters
        { paramK1 = 1.5,
          paramB = \case
            FHoverContents -> 0.7
            FSignature -> 0.5,
          paramFieldWeights = \case
            FHoverContents -> 5
            FSignature -> 10,
          paramFeatureWeights = noFeatures,
          paramFeatureFunctions = noFeatures,
          paramResultsetSoftLimit = 100,
          paramResultsetHardLimit = 200,
          paramAutosuggestPrefilterLimit = 100,
          paramAutosuggestPostfilterLimit = 100
        }
    )
  where
    -- TODO: normalise tokens
    extractTokens Doc {..} FHoverContents = tokenize $ contentsToText $ entry ^. hover . contents
    extractTokens Doc {..} FSignature = tokenize $ signature $ entry ^. otherValues
    contentsToText (HoverContentsMS _) = error "deprecated"
    contentsToText (HoverContents x) = x ^. value
    signature [] = ""
    signature (x : rest) = maybe "" (<> "\n") (x ^? key "signature" . _String) <> signature rest

addDocuments :: (MonadIO m) => Hovercraft -> SearchEngine Doc UUID DocField NoFeatures -> m (HashMap UUID Entry, SearchEngine Doc UUID DocField NoFeatures)
addDocuments hovercraft engine = do
  let es = concatMap (view entries) $ hovercraft ^. pages
  ds <- traverse genDoc es
  pure (HashMap.fromList (map (\d -> (uuid d, entry d)) ds), insertDocs ds engine)

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

app :: FilePath -> Map Text Hovercraft -> HashMap UUID Entry -> SearchEngine Doc UUID DocField NoFeatures -> Application
app staticFilePath hovercrafts kvs engine =
  rewriteRoot "index.html" $ serve api (server staticFilePath hovercrafts kvs engine)