{-# LANGUAGE TemplateHaskell #-}

module Hovercraft (Hovercraft (..), hover, definitions, moniker, rootPath, toDefinition, renderAsMarkdown, prettyDefinition) where

import Control.Lens (imap, (^.))
import Control.Lens.TH
import Data.Aeson
import Language.LSP.Types hiding (line)
import Language.LSP.Types.Lens (character, line, start)
import Relude

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
data Hovercraft = Hovercraft
  { _hover :: Hover,
    _definitions :: [Definition],
    _moniker :: Value,
    _rootPath :: FilePath
  }
  deriving stock (Show, Generic)

instance ToJSON Hovercraft where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON Hovercraft where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

makeLenses ''Hovercraft

-- | Render hovercraft as Markdown
-- Example:
--
-- :::{#label-0}
-- 
-- [🔗](#label-0)
--
-- ```haskell
-- renderAsMarkdown :: [Hovercraft] -> Text
-- ```
--
-- Render hovercraft as Markdown
--
-- Defnitions: file:///home/sisku/src/Hovercraft.hs:39:1
-- 
-- Root path: /home/sisku/
-- :::
renderAsMarkdown :: [Hovercraft] -> Text
renderAsMarkdown hs = unlines $ imap renderSingle hs

renderSingle :: Int -> Hovercraft -> Text
renderSingle idx Hovercraft {_hover = Hover {_contents = contents}, _definitions, _rootPath} =
  unlines
    [ ":::{#label-" <> show idx <> "}",
      "",
      "[🔗](#label-" <> show idx <> ")",
      doc,
      "",
      "Definitions: " <> unlines (map prettyDefinition _definitions),
      "",
      "Root path: " <> toText _rootPath,
      ":::"
    ]
  where
    doc = case contents of
      HoverContents (MarkupContent MkPlainText txt) -> txt
      HoverContents (MarkupContent MkMarkdown txt) -> txt
      HoverContentsMS _ -> error "MarkedString is deprecated"