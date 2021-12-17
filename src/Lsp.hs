{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lsp (buildHovercraft, BuildEnv (..), generateBuildEnv, LspSettings (..)) where

import Colog (LoggerT, Message, logDebug, usingLoggerT)
import Colog.Actions (richMessageAction)
import Control.Lens hiding (List, children, (.=), (??))
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Traversable
import Hovercraft
import Language.LSP.Test
import Language.LSP.Types hiding (Message)
import Language.LSP.Types.Lens hiding (to)
import Relude
import System.Directory.Extra (doesFileExist, makeAbsolute)
import System.FilePath
import System.FilePath.Glob
import System.Process

data BuildEnv = BuildEnv
  { buildEnvCommand :: FilePath,
    buildEnvSourceFilePatterns :: [String],
    buildEnvRootPath :: FilePath,
    buildEnvLanguage :: Text
  }

instance ToJSON BuildEnv where
  toJSON BuildEnv {..} =
    object
      [ "command" .= buildEnvCommand,
        "sourceFilePatterns" .= buildEnvSourceFilePatterns,
        "rootPath" .= buildEnvRootPath,
        "language" .= buildEnvLanguage
      ]

instance FromJSON BuildEnv where
  parseJSON = withObject "BuildEnv" $ \o ->
    BuildEnv
      <$> o .: "command"
      <*> o .: "sourceFilePatterns"
      <*> o .: "rootPath"
      <*> o .: "language"

-- * Build hovercrafts via LSP

buildHovercraft :: BuildEnv -> IO [Hovercraft]
buildHovercraft BuildEnv {..} = do
  files <- concat <$> traverse glob buildEnvSourceFilePatterns
  (Just hin, Just hout, _, _) <- createProcess (shell buildEnvCommand) {std_in = CreatePipe, std_out = CreatePipe}
  hSetBuffering hin NoBuffering
  hSetBuffering hout NoBuffering
  let config = defaultConfig
  hovercrafts <-
    runSessionWithHandles hin hout config fullCaps buildEnvRootPath $ do
      usingLoggerT richMessageAction $ for files seekFile
  pure $ concat hovercrafts
  where
    seekFile :: FilePath -> LoggerT Message Session [Hovercraft]
    seekFile file = do
      logDebug $ toText $ "Seeking file " <> file
      doc <- lift $ openDoc (makeRelative buildEnvRootPath file) buildEnvLanguage
      logDebug $ toText $ "Opened " <> file
      -- wait until the server is ready
      logDebug "Waiting for diagnostics"
      -- TODO: This is a hack, we should make clear why we need to wait for diagnostics and haskell-language-server stops working.
      if buildEnvLanguage == "haskell"
        then pure () -- haskell-language-server hang up when we call waitForDiagnostics
        else void $ lift waitForDiagnostics
      hovercrafts <-
        lift $
          getDocumentSymbols doc >>= \case
            Right symbolInformations -> collectAllHovers' doc (map (view location) symbolInformations)
            Left docSymbols -> collectAllHovers doc docSymbols
      lift $ closeDoc doc
      pure hovercrafts

    collectAllHovers doc docSymbols = concat <$> traverse (collectHover doc) docSymbols
    collectAllHovers' doc docSymbols = concat <$> traverse (collectHover' doc) docSymbols
    collectHover doc docSymbol = do
      let pos = docSymbol ^. selectionRange . start
      hover <- getHover doc pos
      definitions <- getDefinitions doc pos
      case (hover, docSymbol ^. children) of
        (Nothing, Nothing) -> pure []
        (Nothing, Just (List cs)) -> collectAllHovers doc cs
        (Just hover, Nothing) ->
          pure
            [ Hovercraft
                { _hover = hover,
                  _definitions = uncozip definitions,
                  _moniker = Null
                }
            ]
        (Just hover, Just (List cs)) ->
          ( Hovercraft
              { _hover = hover,
                _definitions = uncozip definitions,
                _moniker = Null
              }
              :
          )
            <$> collectAllHovers doc cs
    collectHover' doc docSymbol = do
      let pos = docSymbol ^. range . start
      hover <- getHover doc pos
      definitions <- getDefinitions doc pos
      case hover of
        Nothing -> pure []
        Just hover ->
          pure
            [ Hovercraft
                { _hover = hover,
                  _definitions = uncozip definitions,
                  _moniker = Null
                }
            ]
    uncozip (InL xs) = map InL xs
    uncozip (InR xs) = map InR xs

-- * LSP helpers

newtype LspSettings = LspSettings {unwrapLspSettings :: Map Text LspSetting}
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON LspSettings where
  toJSON = toJSON . unwrapLspSettings

instance FromJSON LspSettings where
  parseJSON = withObject "LspSettings" $ \o ->
    LspSettings <$> (Map.fromList <$> mapM (\(k, v) -> (k,) <$> parseJSON v) (HashMap.toList o))

data LspSetting = LspSetting
  { _lspSettingLanguage :: Text,
    _lspSettingRootUriPatterns :: [Text],
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
      <*> o .: "command"
      <*> o .: "extensions"

-- | Generate a `BuildEnv` from the given file path.
generateBuildEnv :: LspSettings -> FilePath -> IO BuildEnv
generateBuildEnv lspSettings filePath = usingReaderT lspSettings $
  usingLoggerT richMessageAction $ do
    filePath <- liftIO $ makeAbsolute filePath
    language <- detectLanguage filePath
    rootPath <- lift $ searchRootPath language filePath
    let sourceFilePatterns = [rootPath <> "/**/*" <> takeExtension filePath]
    command <- lift $ getCommand language
    pure
      BuildEnv
        { buildEnvCommand = command,
          buildEnvSourceFilePatterns = sourceFilePatterns,
          buildEnvRootPath = rootPath,
          buildEnvLanguage = language
        }

-- | Detect what programming language the given file is written in.
detectLanguage :: MonadReader LspSettings m => FilePath -> LoggerT Message m Text
detectLanguage filePath = do
  logDebug $ toText $ "Detecting language for " <> filePath
  let ext = toText $ takeExtension filePath
  logDebug $ "Looking for language for extension " <> ext
  lspSettings <- Map.elems <$> lift (asks unwrapLspSettings)
  logDebug $ "LspSettings: " <> show lspSettings
  let matches = mapMaybe ?? lspSettings $ \LspSetting {_lspSettingLanguage = language, _lspSettingExtensions = extensions} ->
        if ext `elem` extensions
          then Just language
          else Nothing
  logDebug $ "Detected language: " <> show matches
  case matches of
    [] -> error $ "Could not detect language for " <> show filePath
    [language] -> pure language
    _ -> error $ "Multiple languages detected for " <> show filePath <> "\nTODO: Implement language detection when multiple languages are detected"

-- | Get the root path of the given file.
searchRootPath :: (MonadIO m, MonadReader LspSettings m) => Text -> FilePath -> m String
searchRootPath language filePath = do
  lspSetting <- fromMaybe (error $ "Language " <> show language <> " not found") . view (at language) <$> asks unwrapLspSettings
  let rootPathPatterns = _lspSettingRootUriPatterns lspSetting
  findRootPath rootPathPatterns [takeDirectory filePath]
  where
    findRootPath _ [] = error $ "Could not find root path for " <> show filePath
    findRootPath rootPathPatterns (path : paths) = do
      isRootPath <- anyM (\rootPathPattern -> liftIO $ doesFileExist $ path <> "/" <> toString rootPathPattern) rootPathPatterns
      if isRootPath
        then pure path
        else findRootPath rootPathPatterns $ case joinPath <$> viaNonEmpty init (splitPath path) of
          Just parentPath | isValid parentPath && parentPath `notElem` paths -> parentPath : paths
          _ -> paths

-- | Get the command to run the Language Server.
getCommand :: MonadReader LspSettings m => Text -> m String
getCommand language = do
  lspSetting <- view (to unwrapLspSettings . at language)
  case lspSetting of
    Nothing -> error $ "Language " <> show language <> " not found"
    Just lspSetting -> pure $ toString $ _lspSettingCommand lspSetting