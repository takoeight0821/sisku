{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Sisku.Hovercraft where

import Control.Lens ((^.))
import Control.Lens.TH
import Data.Aeson
import Language.LSP.Types hiding (line)
import Language.LSP.Types.Lens
import Relude
import Sisku.Config (HasProjectId (..))
import Sisku.Token

data Definition = Definition {_uri :: Uri, _range :: Range}
  deriving stock (Eq, Show, Generic)

fromLocation :: Location -> Definition
fromLocation Location {_uri, _range} = Definition _uri _range

fromLocationLink :: LocationLink -> Definition
fromLocationLink LocationLink {_targetUri, _targetRange} = Definition _targetUri _targetRange

toDefinition :: Location |? LocationLink -> Definition
toDefinition (InL loc@Location {}) = fromLocation loc
toDefinition (InR loc@LocationLink {}) = fromLocationLink loc

prettyDefinition :: Definition -> Text
prettyDefinition Definition {_uri, _range} =
  getUri _uri <> ":" <> show (_range ^. start . line) <> ":" <> show (_range ^. start . character)

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
    _signatureToken :: [[Token]],
    _otherValues :: [Value],
    _rootPath :: FilePath
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Entry where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON Entry where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

makeFieldsNoPrefix ''Entry

newtype Page = Page {_entries :: [Entry]}
  deriving stock (Eq, Show, Generic)

instance ToJSON Page where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON Page where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

makeFieldsNoPrefix ''Page

pageDocument :: Page -> TextDocumentIdentifier
pageDocument Page {_entries = e : _} = _document e
pageDocument _ = error "pageDocument: no entries"

data Hovercraft = Hovercraft {_projectId :: Text, _pages :: [Page]}
  deriving stock (Eq, Show, Generic)

instance ToJSON Hovercraft where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON Hovercraft where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

makeFieldsNoPrefix ''Hovercraft