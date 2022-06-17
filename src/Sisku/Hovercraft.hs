{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Sisku.Hovercraft where

import Codec.Serialise
import Control.Lens ((^.))
import Control.Lens.TH
import Data.Aeson hiding (Encoding, decode, encode)
import qualified Data.Mod.Word
import Language.LSP.Types hiding (line)
import Language.LSP.Types.Lens
import Relude
import Sisku.Config (HasProjectId (..))
import Sisku.Lsp ()
import Sisku.Token
import Sisku.Tree.Type (Tree)
import Text.PrettyPrint.HughesPJClass (Pretty (..))

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

instance Serialise Definition

-- | Hover document and definition information
data Entry = Entry
  { _document :: TextDocumentIdentifier,
    _projectId :: Text,
    _hover :: Hover,
    _definitions :: [Definition],
    _signatureToken :: [(Text, [WithPos Token])],
    _typeTree :: [Tree],
    _rootPath :: FilePath
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Entry where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON Entry where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

instance Pretty Entry where
  pPrint Entry {..} = pPrint _hover

instance Serialise TextDocumentIdentifier

deriving stock instance Generic TextDocumentIdentifier

instance Serialise Uri

instance Serialise Hover

deriving stock instance Generic Hover

instance Serialise HoverContents

deriving stock instance Generic HoverContents

instance Serialise a => Serialise (List a)

instance Serialise MarkupContent

deriving stock instance Generic MarkupContent

instance Serialise MarkedString

deriving stock instance Generic MarkedString

instance Serialise LanguageString

deriving stock instance Generic LanguageString

instance Serialise MarkupKind

deriving stock instance Generic MarkupKind

instance Serialise Range

instance Serialise Position

instance Serialise UInt

instance Serialise (Data.Mod.Word.Mod 2147483648)

instance Serialise Entry

makeFieldsNoPrefix ''Entry

newtype Page = Page {_entries :: [Entry]}
  deriving stock (Eq, Show, Generic)

instance ToJSON Page where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON Page where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

instance Serialise Page

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

instance Serialise Hovercraft

makeFieldsNoPrefix ''Hovercraft