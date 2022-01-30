{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lsp (buildHovercraft, BuildEnv (..), generateBuildEnv, LspSettings (..)) where

import Colog (LoggerT, Message, logDebug, logError, logInfo, usingLoggerT)
import Colog.Actions (richMessageAction)
import Control.Exception (catch)
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
import System.Time.Extra (sleep)

data BuildEnv = BuildEnv
  { buildEnvCommand :: FilePath,
    buildEnvSourceFiles :: [FilePath],
    buildEnvRootPath :: FilePath,
    buildEnvLanguage :: Text
  }

instance ToJSON BuildEnv where
  toJSON BuildEnv {..} =
    object
      [ "command" .= buildEnvCommand,
        "sourceFiles" .= buildEnvSourceFiles,
        "rootPath" .= buildEnvRootPath,
        "language" .= buildEnvLanguage
      ]

instance FromJSON BuildEnv where
  parseJSON = withObject "BuildEnv" $ \o ->
    BuildEnv
      <$> o .: "command"
      <*> o .: "sourceFiles"
      <*> o .: "rootPath"
      <*> o .: "language"

-- * Build hovercrafts via LSP

buildHovercraft :: BuildEnv -> IO Hovercraft
buildHovercraft env@BuildEnv {..} = do
  let config = defaultConfig
  hovercrafts <- for buildEnvSourceFiles $ seekFile config 0
  pure $ Hovercraft hovercrafts
  where
    seekFile config waitSec file
      | waitSec <= 10 =
        runSessionWithConfig config buildEnvCommand fullCaps buildEnvRootPath (usingLoggerT richMessageAction $ seekFile' waitSec file)
          `catch` \(e :: SessionException) -> do
            usingLoggerT richMessageAction $ logError $ "session error: " <> show e
            seekFile config (waitSec + 1) file
      | otherwise = error $ "Cannot connect to LSP server: " <> show file
    seekFile' :: Double -> FilePath -> LoggerT Message Session Page
    seekFile' waitSec file = do
      logDebug $ toText $ "Seeking file " <> file
      doc <- lift $ openDoc (makeRelative buildEnvRootPath file) buildEnvLanguage
      logDebug $ toText $ "Opened " <> file
      -- wait until the server is ready
      logInfo $ "waiting " <> show waitSec <> " seconds before requesting hover"
      liftIO $ sleep waitSec
      
      logDebug "Get semantic tokens"
      semanticTokens <- lift $ getSemanticTokens doc
      logDebug $ "Semantic tokens: " <> show semanticTokens

      entries <-
        lift $
          getDocumentSymbols doc >>= \case
            Right symInfos -> craft env doc symInfos
            Left docSymbols -> craft env doc docSymbols
      lift $ closeDoc doc
      pure $ Page { _document = doc, _entries = entries }

class Craftable a where
  craft :: BuildEnv -> TextDocumentIdentifier -> a -> Session [Entry]

instance Craftable a => Craftable [a] where
  craft env doc xs = concat <$> traverse (craft env doc) xs

instance Craftable DocumentSymbol where
  craft env@BuildEnv {..} doc DocumentSymbol {..} = do
    let pos = _selectionRange ^. start
    hover <- getHover doc pos
    definitions <- getDefinitions doc pos
    case (hover, _children) of
      (Nothing, Nothing) -> pure []
      (Nothing, Just (List cs)) -> craft env doc cs
      (Just hover, Nothing) ->
        pure
          [ Entry
              { _hover = hover,
                _definitions = map toDefinition $ uncozip definitions,
                _moniker = Null,
                _rootPath = buildEnvRootPath
              }
          ]
      (Just hover, Just (List cs)) ->
        ( Entry
            { _hover = hover,
              _definitions = map toDefinition $ uncozip definitions,
              _moniker = Null,
              _rootPath = buildEnvRootPath
            }
            :
        )
          <$> craft env doc cs

instance Craftable SymbolInformation where
  craft BuildEnv {..} doc SymbolInformation {..} = do
    let pos = _location ^. range . start
    hover <- getHover doc pos
    definitions <- getDefinitions doc pos
    case hover of
      Nothing -> pure []
      Just hover ->
        pure
          [ Entry
              { _hover = hover,
                _definitions = map toDefinition $ uncozip definitions,
                _moniker = Null,
                _rootPath = buildEnvRootPath
              }
          ]

uncozip :: ([a] |? [b]) -> [a |? b]
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

-- | Generate a `BuildEnv` from the given file path.
generateBuildEnv :: LspSettings -> FilePath -> IO BuildEnv
generateBuildEnv lspSettings filePath = usingReaderT lspSettings $
  usingLoggerT richMessageAction $ do
    filePath <- liftIO $ makeAbsolute filePath
    language <- detectLanguage filePath
    rootPath <- lift $ searchRootPath language filePath
    sourceFiles <- liftIO $ fmap normalise <$> glob (rootPath <> "/**/*" <> takeExtension filePath)
    sourceFiles <- lift $ filterExcluded language sourceFiles
    command <- lift $ getCommand language
    pure
      BuildEnv
        { buildEnvCommand = command,
          buildEnvSourceFiles = sourceFiles,
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

-- | Exclude files based on _lspSettingExcludePatterns.
filterExcluded :: (MonadReader LspSettings m, MonadIO m) => Text -> [FilePath] -> m [FilePath]
filterExcluded language sourceFiles = do
  lspSetting <- fromMaybe (error $ "Language " <> show language <> " not found") . view (at language) <$> asks unwrapLspSettings
  excludedFiles <- liftIO $ traverse makeAbsolute . concat =<< traverse (glob . toString) (_lspSettingExcludePatterns lspSetting)
  pure $ filter (`notElem` excludedFiles) sourceFiles

-- | Get the command to run the Language Server.
getCommand :: MonadReader LspSettings m => Text -> m String
getCommand language = do
  lspSetting <- view (to unwrapLspSettings . at language)
  case lspSetting of
    Nothing -> error $ "Language " <> show language <> " not found"
    Just lspSetting -> pure $ toString $ _lspSettingCommand lspSetting