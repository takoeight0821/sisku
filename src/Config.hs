{-# LANGUAGE TemplateHaskell #-}

module Config where

import Control.Lens.TH (makeFieldsNoPrefix)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map as Map
import Language.LSP.Types.Lens
import Relude

-- | Configuration for Sisku.
data SiskuConfig = SiskuConfig
  { _projectId :: Text,
    _lspSettings :: LspSettings
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON SiskuConfig where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON SiskuConfig where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

-- | Load configuration from file.
loadConfig :: FilePath -> IO SiskuConfig
loadConfig configFile =
  readFileLBS configFile
    >>= ( \case
            Left err -> error $ "Failed to parse config file: " <> toText err
            Right config -> pure config
        )
      . eitherDecode

-- | LSP settings for corresponding language.
newtype LspSettings = LspSettings {unwrapLspSettings :: Map Text LspSetting}
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON LspSettings where
  toJSON = toJSON . unwrapLspSettings

instance FromJSON LspSettings where
  parseJSON = withObject "LspSettings" $ \o ->
    LspSettings <$> (Map.fromList <$> mapM (\(k, v) -> (Key.toText k,) <$> parseJSON v) (KeyMap.toList o))

-- | Configuration for LSP.
data LspSetting = LspSetting
  { _language :: Text,
    _rootUriPatterns :: [Text],
    _excludePatterns :: [Text],
    _command :: Text,
    _extensions :: [Text]
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON LspSetting where
  toJSON LspSetting {..} =
    object
      [ "language" .= _language,
        "root_uri_patterns" .= _rootUriPatterns,
        "exclude_patterns" .= _excludePatterns,
        "command" .= _command,
        "extensions" .= _extensions
      ]

instance FromJSON LspSetting where
  parseJSON = withObject "LspSetting" $ \v -> do
    _language <- v .: "language"
    _rootUriPatterns <- v .: "root_uri_patterns"
    _excludePatterns <- v .: "exclude_patterns"
    _command <- v .: "command"
    _extensions <- v .: "extensions"
    pure LspSetting {..}

makeFieldsNoPrefix ''SiskuConfig
makeFieldsNoPrefix ''LspSetting
