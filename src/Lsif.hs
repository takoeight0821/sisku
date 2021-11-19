-- | LSIF type definitions
module Lsif where

import Data.Aeson
import Language.LSP.Types
import Relude

-- * LSIF

type Element = Value

newtype HoverResult = HoverResult
  { result :: Hover
  }
  deriving stock (Show, Generic)

instance ToJSON HoverResult

instance FromJSON HoverResult
