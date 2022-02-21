{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lsp (buildHovercraft, BuildEnv (..), generateBuildEnv, LspSettings (..)) where

import Colog (HasLog (..), LoggerT, Message, logDebug, logInfo, logWarning, usingLoggerT)
import Colog.Actions (richMessageAction)
import Config
import Control.Lens hiding (List, children, (.=), (??))
import Data.Aeson
import qualified Data.Map as Map
import Data.Traversable
import Hovercraft
import Language.LSP.Test hiding (getDefinitions, getDocumentSymbols, getHover)
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

-- | Build a hovercraft using LSP.
buildHovercraft :: BuildEnv -> IO Hovercraft
buildHovercraft env@BuildEnv {..} = do
  let config = defaultConfig
  hovercrafts <-
    runSessionWithConfig config buildEnvCommand fullCaps buildEnvRootPath $
      usingLoggerT richMessageAction $ do
        fileList <- for buildEnvSourceFiles $ \file -> do
          doc <- lift $ openDoc (makeRelative buildEnvRootPath file) buildEnvLanguage
          logDebug $ toText $ "Opened " <> file
          pure (file, doc)
        for fileList $ uncurry seekFile
  pure $ Hovercraft hovercrafts
  where
    seekFile :: FilePath -> TextDocumentIdentifier -> LoggerT Message Session Page
    seekFile file doc = do
      logDebug $ toText $ "Seeking file " <> file
      symbols <- getDocumentSymbols doc
      logInfo $ "Got symbols for " <> show file
      entries <-
        case symbols of
          Right symInfos -> craft env doc symInfos
          Left docSymbols -> craft env doc docSymbols
      lift $ closeDoc doc
      pure $ Page {_document = doc, _entries = entries}

getDocumentSymbols ::
  ( MonadReader env (t Session),
    MonadIO (t Session),
    MonadTrans t,
    HasLog env Message (t Session)
  ) =>
  TextDocumentIdentifier ->
  t Session (Either [DocumentSymbol] [SymbolInformation])
getDocumentSymbols doc = do
  ResponseMessage _ rspLid res <- lift $ request STextDocumentDocumentSymbol (DocumentSymbolParams Nothing Nothing doc)
  case res of
    Right (InL (List xs)) -> pure (Left xs)
    Right (InR (List xs)) -> pure (Right xs)
    Left err -> do
      logWarning $ "Error id " <> show rspLid <> ": " <> show err
      liftIO $ sleep 1
      logWarning "Retrying..."
      getDocumentSymbols doc

class Craftable a where
  craft :: BuildEnv -> TextDocumentIdentifier -> a -> LoggerT Message Session [Entry]

instance Craftable a => Craftable [a] where
  craft env doc xs = concat <$> traverse (craft env doc) xs

getHover ::
  ( MonadReader env (t Session),
    MonadIO (t Session),
    MonadTrans t,
    HasLog env Message (t Session)
  ) =>
  TextDocumentIdentifier ->
  Position ->
  t Session (Maybe Hover)
getHover doc pos = do
  let params = HoverParams doc pos Nothing
  ResponseMessage _ rspLid res <- lift $ request STextDocumentHover params
  case res of
    Right x -> pure x
    Left err -> do
      logWarning $ "Error id " <> show rspLid <> ": " <> show err
      liftIO $ sleep 1
      logWarning "Retrying..."
      getHover doc pos

getDefinitions ::
  ( MonadReader env (t Session),
    MonadIO (t Session),
    MonadTrans t,
    HasLog env Message (t Session)
  ) =>
  TextDocumentIdentifier ->
  Position ->
  t Session ([Location] |? [LocationLink])
getDefinitions doc pos = do
  let params = DefinitionParams doc pos Nothing Nothing
  ResponseMessage _ rspLid res <- lift $ request STextDocumentDefinition params
  case res of
    Right (InL loc) -> pure (InL [loc])
    Right (InR (InL (List locs))) -> pure (InL locs)
    Right (InR (InR (List locLinks))) -> pure (InR locLinks)
    Left err -> do
      logWarning $ "Error id " <> show rspLid <> ": " <> show err
      liftIO $ sleep 1
      logWarning "Retrying..."
      getDefinitions doc pos

instance Craftable DocumentSymbol where
  craft env@BuildEnv {..} doc DocumentSymbol {..} = do
    let pos = over character (+ 1) $ _selectionRange ^. start
    hover <- getHover doc pos
    definitions <- getDefinitions doc pos

    case (hover, _children) of
      (Nothing, Nothing) -> pure []
      (Nothing, Just (List cs)) -> craft env doc cs
      (Just hover, Nothing) -> do
        pure
          [ Entry
              { _hover = hover,
                _definitions = map toDefinition $ uncozip definitions,
                _moniker = Null,
                _rootPath = buildEnvRootPath
              }
          ]
      (Just hover, Just (List cs)) -> do
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
      Just hover -> do
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

-- | Generate a `BuildEnv` from the given file path.
generateBuildEnv :: SiskuConfig -> FilePath -> IO BuildEnv
generateBuildEnv SiskuConfig {_lspSettings = lspSettings} filePath = usingReaderT lspSettings $
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