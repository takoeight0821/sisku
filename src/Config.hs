{-# LANGUAGE TemplateHaskell #-}
module Config where

import Relude
import Data.Aeson
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Control.Lens.TH ( makeLenses )

-- | Configuration for Sisku.
data SiskuConfig = SiskuConfig
  { _projectId :: Text
  , _lspSettings :: LspSettings
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON SiskuConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON SiskuConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

-- | Load configuration from file.
loadConfig :: FilePath -> IO SiskuConfig
loadConfig configFile = do
  contents <- readFileLBS configFile
  case eitherDecode contents of
    Left err -> error $ "Failed to parse config file: " <> toText err
    Right config -> pure config 

-- | LSP settings for corresponding language.
newtype LspSettings = LspSettings {unwrapLspSettings :: Map Text LspSetting}
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON LspSettings where
  toJSON = toJSON . unwrapLspSettings

instance FromJSON LspSettings where
  parseJSON = withObject "LspSettings" $ \o ->
    LspSettings <$> (Map.fromList <$> mapM (\(k, v) -> (k,) <$> parseJSON v) (HashMap.toList o))

-- | Configuration for LSP.
data LspSetting = LspSetting
  { _lspSettingLanguage :: Text,
    _lspSettingRootUriPatterns :: [Text],
    _lspSettingExcludePatterns :: [Text],
    _lspSettingCommand :: Text,
    _lspSettingExtensions :: [Text]
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON LspSetting where
  toJSON LspSetting {..} =
    object
      [ "language" .= _lspSettingLanguage,
        "root_uri_patterns" .= _lspSettingRootUriPatterns,
        "command" .= _lspSettingCommand,
        "extensions" .= _lspSettingExtensions
      ]

instance FromJSON LspSetting where
  parseJSON = withObject "LspSetting" $ \o ->
    LspSetting
      <$> o .: "language"
      <*> o .: "root_uri_patterns"
      <*> o .: "exclude_patterns"
      <*> o .: "command"
      <*> o .: "extensions"

makeLenses ''SiskuConfig
