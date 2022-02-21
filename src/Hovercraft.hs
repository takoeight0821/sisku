{-# LANGUAGE TemplateHaskell #-}

module Hovercraft (Hovercraft (..), Page (..), Entry (..), entries, toDefinition, prettyDefinition, writeHovercraft, pageDocument) where

import Config
import Control.Lens (imap, (^.))
import Control.Lens.TH
import Data.Aeson
import qualified Data.Aeson as Aeson
import Language.LSP.Types hiding (line)
import Language.LSP.Types.Lens (character, line, start)
import Relude
import System.Directory.Extra (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))

data Definition = Definition {_uri :: Uri, _range :: Range}
  deriving stock (Show, Generic)

fromLocation :: Location -> Definition
fromLocation Location {_uri = uri, _range = range} = Definition uri range

fromLocationLink :: LocationLink -> Definition
fromLocationLink LocationLink {_targetUri = uri, _targetRange = range} = Definition uri range

toDefinition :: Location |? LocationLink -> Definition
toDefinition (InL loc@Location {}) = fromLocation loc
toDefinition (InR loc@LocationLink {}) = fromLocationLink loc

prettyDefinition :: Definition -> Text
prettyDefinition Definition {_uri = uri, _range = range} =
  getUri uri <> ":" <> show (range ^. start . line) <> ":" <> show (range ^. start . character)

instance ToJSON Definition where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON Definition where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

-- | Hover document and definition information
data Entry = Entry
  { _document :: TextDocumentIdentifier,
    _projectId :: Text,
    _hover :: Hover,
    _definitions :: [Definition],
    _moniker :: Value,
    _rootPath :: FilePath
  }
  deriving stock (Show, Generic)

instance ToJSON Entry where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON Entry where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

newtype Page = Page {_entries :: [Entry]}
  deriving stock (Show, Generic)

instance ToJSON Page where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON Page where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

makeLenses ''Page

pageDocument :: Page -> TextDocumentIdentifier
pageDocument Page {_entries = e : _} = _document e
pageDocument _ = error "pageDocument: no entries"

data Hovercraft = Hovercraft {projectId :: Text, pages :: [Page]}
  deriving stock (Show, Generic)

instance ToJSON Hovercraft

instance FromJSON Hovercraft

-- | Get XDG_DATA_HOME
getDataHome :: IO FilePath
getDataHome = getXdgDirectory XdgData "sisku/hovercraft"

-- | Write hovercraft to file
writeHovercraft :: SiskuConfig -> Hovercraft -> IO ()
writeHovercraft config hc = do
  dataHome <- getDataHome
  createDirectoryIfMissing True dataHome
  let file = dataHome </> (toString (config ^. Config.projectId) <> ".json")
  Aeson.encodeFile file hc