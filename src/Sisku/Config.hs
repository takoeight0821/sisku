{-# LANGUAGE TemplateHaskell #-}

module Sisku.Config where

import Control.Lens.TH (makeFieldsNoPrefix)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map qualified as Map
import Language.LSP.Types.Lens
import Relude

-- | Configuration for Sisku.
data Config = Config
  { _projectId :: Text,
    _lspSettingMap :: LspSettingMap
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON Config where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

-- | Load configuration from file.
loadConfig :: FilePath -> IO Config
loadConfig configFile =
  readFileLBS configFile
    >>= ( \case
            Left err -> error $ "Failed to parse config file: " <> toText err
            Right config -> pure config
        )
      . eitherDecode

-- | LSP settings for corresponding language.
newtype LspSettingMap = LspSettingMap {unwrapLspSettingMap :: Map Text LspSetting}
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON LspSettingMap where
  toJSON = toJSON . unwrapLspSettingMap

instance FromJSON LspSettingMap where
  parseJSON = withObject "LspSettingMap" $ \o ->
    LspSettingMap <$> (Map.fromList <$> mapM (\(k, v) -> (Key.toText k,) <$> parseJSON v) (KeyMap.toList o))

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

makeFieldsNoPrefix ''Config
makeFieldsNoPrefix ''LspSetting
