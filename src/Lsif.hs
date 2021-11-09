-- | LSIF type definitions
module Lsif where

import Data.Aeson
import Relude

-- * LSP

data Position = Position
  { -- | Line position in a document (zero-based).
    line :: Int,
    -- | Character offset on a line in a document (zero-based). Assuming that
    -- the line is represented as a string, the `character` value represents
    -- the gap between the `character` and `character + 1`.
    --
    -- If the character value is greater than the line length it defaults back
    -- to the line length.
    character :: Int
  }
  deriving stock (Show, Generic)

instance ToJSON Position

instance FromJSON Position

data Range = Range
  { -- | The range's start position.
    start :: Position,
    -- | The range's end position.
    end :: Position
  }
  deriving stock (Show, Generic)

instance ToJSON Range

instance FromJSON Range

data MarkupKind = Plaintext | Markdown
  deriving stock (Show, Generic)

instance ToJSON MarkupKind where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = camelTo2 '_'}

instance FromJSON MarkupKind where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = camelTo2 '_'}

-- |
--  A `MarkupContent` literal represents a string value which content is
--  interpreted base on its kind flag. Currently the protocol supports
--  `plaintext` and `markdown` as markup kinds.
--
--  If the kind is `markdown` then the value can contain fenced code blocks like
--  in GitHub issues.
--
--  Here is an example how such a string can be constructed using
--  JavaScript / TypeScript:
--  ```typescript
--  let markdown: MarkdownContent = {
--  	kind: MarkupKind.Markdown,
--  	value: [
--  		'# Header',
--  		'Some text',
--  		'```typescript',
--  		'someCode();',
--  		'```'
--  	].join('\n')
--  };
--  ```
--
--  *Please Note* that clients might sanitize the return markdown. A client could
--  decide to remove HTML from the markdown to avoid script execution.
data MarkupContent = MarkupContent
  { -- | The type of the Markup
    kind :: MarkupKind,
    -- | The content itself
    value :: String
  }
  deriving stock (Show, Generic)

instance ToJSON MarkupContent

instance FromJSON MarkupContent

-- | The result of a hover request.
data Hover = Hover
  { contents :: MarkupContent,
    -- | An optional range is a range inside a text document
    -- that is used to visualize a hover, e.g. by changing the background color.
    range :: Maybe Range
  }
  deriving stock (Show, Generic)

instance ToJSON Hover

instance FromJSON Hover

-- * LSIF

type Element = Value

newtype HoverResult = HoverResult
  { result :: Hover
  }
  deriving stock (Show, Generic)

instance ToJSON HoverResult

instance FromJSON HoverResult
