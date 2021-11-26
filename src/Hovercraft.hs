{-# LANGUAGE TemplateHaskell #-}

module Hovercraft (Hovercraft(..), hover, definitions, moniker) where

import Control.Lens.TH
import Data.Aeson
import Language.LSP.Types
import Relude

-- | Hover document and definition information
data Hovercraft = Hovercraft
  { _hover :: Hover,
    _definitions :: [Location |? LocationLink],
    _moniker :: Value
  }
  deriving stock (Show, Generic)

instance ToJSON Hovercraft where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON Hovercraft where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

makeLenses ''Hovercraft
