{-# LANGUAGE TemplateHaskell #-}

module Hovercraft (Hovercraft (..), hover, definitions, moniker, renderAsMarkdown) where

import Control.Lens (imap)
import Control.Lens.TH
import Data.Aeson
import Language.LSP.Types
import Relude

-- | Hover document and definition information
data Hovercraft = Hovercraft
  { _hover :: Hover,
    _definitions :: [Location |? LocationLink],
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
-- ```haskell
-- renderAsMarkdown :: [Hovercraft] -> Text
-- ```
--
-- Render hovercraft as Markdown
--
-- Defined in: file:///home/sisku/src/Hovercraft.hs:39:1
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
      "Definitions: " <> show _definitions,
      "",
      "Root path: " <> toText _rootPath,
      ":::"
    ]
  where
    doc = case contents of
      HoverContents (MarkupContent MkPlainText txt) -> txt
      HoverContents (MarkupContent MkMarkdown txt) -> txt
      HoverContentsMS _ -> error "MarkedString is deprecated"