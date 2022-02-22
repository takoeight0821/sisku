{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lsp (buildHovercraft, BuildEnv (..), generateBuildEnv, LspSettings (..)) where

import Colog (HasLog (..), LoggerT, Message, logDebug, logInfo, logWarning, usingLoggerT)
import Colog.Actions (richMessageAction)
import Config
import Control.Lens hiding (List, children, (.=), (??))
import Data.Aeson
import qualified Data.Map as Map
import Data.Traversable
import Hovercraft hiding (definitions)
import Language.LSP.Test hiding (getDefinitions, getDocumentSymbols, getHover)
import Language.LSP.Types hiding (Message)
import Language.LSP.Types.Lens hiding (hover, to)
import Relude
import System.Directory.Extra (doesFileExist, makeAbsolute)
import System.FilePath
import System.FilePath.Glob
import System.Time.Extra (sleep)

data BuildEnv = BuildEnv
  { _command :: FilePath,
    _sourceFiles :: [FilePath],
    _rootPath :: FilePath,
    _language :: Text,
    _projectId :: Text
  }
  deriving stock (Generic)

instance ToJSON BuildEnv where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON BuildEnv where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

makeFieldsNoPrefix ''BuildEnv

-- * Build hovercrafts via LSP

-- | Build a hovercraft using LSP.
buildHovercraft :: BuildEnv -> IO Hovercraft
buildHovercraft env = do
  let config = defaultConfig
  hovercrafts <-
    runSessionWithConfig config (env ^. command) fullCaps (env ^. rootPath) $
      usingLoggerT richMessageAction $ do
        fileList <- for (env ^. sourceFiles) $ \file -> do
          doc <- lift $ openDoc (makeRelative (env ^. rootPath) file) (env ^. language)
          logDebug $ toText $ "Opened " <> file
          pure (file, doc)
        for fileList $ uncurry seekFile
  pure $ Hovercraft (env ^. projectId) hovercrafts
  where
    seekFile :: FilePath -> TextDocumentIdentifier -> LoggerT Message Session Page
    seekFile file doc = do
      logDebug $ toText $ "Seeking file " <> file
      symbols <- getDocumentSymbols doc
      logInfo $ "Got symbols for " <> show file
      page <-
        Page
          <$> case symbols of
            Right symInfos -> craft env doc symInfos
            Left docSymbols -> craft env doc docSymbols
      lift $ closeDoc doc
      pure page

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
  ResponseMessage _ rspLid res <- lift $ request STextDocumentHover (HoverParams doc pos Nothing)
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
  ResponseMessage _ rspLid res <- lift $ request STextDocumentDefinition (DefinitionParams doc pos Nothing Nothing)
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
  craft env doc DocumentSymbol {..} = do
    let pos = over character (+ 1) $ _selectionRange ^. start
    mhover <- getHover doc pos
    definitions <- getDefinitions doc pos

    case (mhover, _children) of
      (Nothing, Nothing) -> pure []
      (Nothing, Just (List cs)) -> craft env doc cs
      (Just hover, Nothing) -> do
        pure
          [ Entry
              { _document = doc,
                _projectId = env ^. projectId,
                _hover = hover,
                _definitions = map toDefinition $ uncozip definitions,
                _moniker = Null,
                _rootPath = env ^. rootPath
              }
          ]
      (Just hover, Just (List cs)) -> do
        ( Entry
            { _document = doc,
              _projectId = env ^. projectId,
              _hover = hover,
              _definitions = map toDefinition $ uncozip definitions,
              _moniker = Null,
              _rootPath = env ^. rootPath
            }
            :
          )
          <$> craft env doc cs

instance Craftable SymbolInformation where
  craft env doc SymbolInformation {..} = do
    let pos = _location ^. range . start
    mhover <- getHover doc pos
    definitions <- getDefinitions doc pos
    case mhover of
      Nothing -> pure []
      Just hover -> do
        pure
          [ Entry
              { _document = doc,
                _projectId = env ^. projectId,
                _hover = hover,
                _definitions = map toDefinition $ uncozip definitions,
                _moniker = Null,
                _rootPath = env ^. rootPath
              }
          ]

uncozip :: ([a] |? [b]) -> [a |? b]
uncozip (InL xs) = map InL xs
uncozip (InR xs) = map InR xs

-- * LSP helpers

-- | Generate a `BuildEnv` from the given file path.
generateBuildEnv :: SiskuConfig -> FilePath -> IO BuildEnv
generateBuildEnv SiskuConfig {_projectId, _lspSettings} filePath = usingReaderT _lspSettings $
  usingLoggerT richMessageAction $ do
    filePath <- liftIO $ makeAbsolute filePath
    _language <- detectLanguage filePath
    _rootPath <- lift $ searchRootPath _language filePath
    _sourceFiles <- fmap normalise <$> liftIO (glob (_rootPath <> "/**/*" <> takeExtension filePath))
    _sourceFiles <- lift $ filterExcluded _language _sourceFiles
    _command <- lift $ getCommand _language
    pure BuildEnv {..}

-- | Detect what programming language the given file is written in.
detectLanguage :: MonadReader LspSettings m => FilePath -> LoggerT Message m Text
detectLanguage filePath = do
  logDebug $ toText $ "Detecting language for " <> filePath
  let ext = toText $ takeExtension filePath
  logDebug $ "Looking for language for extension " <> ext
  lspSettings <- Map.elems <$> lift (asks unwrapLspSettings)
  logDebug $ "LspSettings: " <> show lspSettings
  let matches = mapMaybe ?? lspSettings $ \LspSetting {_language = language, _extensions = extensions} ->
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
  findRootPath (lspSetting ^. rootUriPatterns) [takeDirectory filePath]
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
  excludedFiles <- liftIO $ traverse makeAbsolute . concat =<< traverse (glob . toString) (lspSetting ^. excludePatterns)
  pure $ filter (`notElem` excludedFiles) sourceFiles

-- | Get the command to run the Language Server.
getCommand :: MonadReader LspSettings m => Text -> m String
getCommand language = do
  lspSetting <- view (to unwrapLspSettings . at language)
  case lspSetting of
    Nothing -> error $ "Language " <> show language <> " not found"
    Just lspSetting -> pure $ toString $ lspSetting ^. command