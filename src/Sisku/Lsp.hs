{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Sisku.Lsp (getDocumentSymbols, getHover, getDefinitions, uncozip, detectLanguage, searchRootPath, filterExcluded, getCommand) where

import Control.Lens hiding (List, children, (.=), (??))
import qualified Data.Map as Map
import Language.LSP.Test hiding (getDefinitions, getDocumentSymbols, getHover)
import Language.LSP.Types hiding (Message)
import Language.LSP.Types.Lens hiding (hover, to)
import Relude
import Sisku.App
import Sisku.Config
import System.Directory.Extra (doesFileExist, makeAbsolute)
import System.FilePath
import System.FilePath.Glob
import System.Time.Extra (sleep)

getDocumentSymbols :: TextDocumentIdentifier -> Session (Either [DocumentSymbol] [SymbolInformation])
getDocumentSymbols doc = do
  ResponseMessage _ rspLid res <- request STextDocumentDocumentSymbol (DocumentSymbolParams Nothing Nothing doc)
  case res of
    Right (InL (List xs)) -> pure (Left xs)
    Right (InR (List xs)) -> pure (Right xs)
    Left err -> do
      putTextLn $ "Error id " <> show rspLid <> ": " <> show err
      liftIO $ sleep 1
      putTextLn "Retrying..."
      getDocumentSymbols doc

getHover :: TextDocumentIdentifier -> Position -> Session (Maybe Hover)
getHover doc pos = do
  ResponseMessage _ rspLid res <- request STextDocumentHover (HoverParams doc pos Nothing)
  case res of
    Right x -> pure x
    Left err -> do
      putTextLn $ "Error id " <> show rspLid <> ": " <> show err
      liftIO $ sleep 1
      putTextLn "Retrying..."
      getHover doc pos

getDefinitions :: TextDocumentIdentifier -> Position -> Session ([Location] |? [LocationLink])
getDefinitions doc pos = do
  ResponseMessage _ rspLid res <- request STextDocumentDefinition (DefinitionParams doc pos Nothing Nothing)
  case res of
    Right (InL loc) -> pure (InL [loc])
    Right (InR (InL (List locs))) -> pure (InL locs)
    Right (InR (InR (List locLinks))) -> pure (InR locLinks)
    Left err -> do
      putTextLn $ "Error id " <> show rspLid <> ": " <> show err
      liftIO $ sleep 1
      putTextLn "Retrying..."
      getDefinitions doc pos

uncozip :: ([a] |? [b]) -> [a |? b]
uncozip (InL xs) = map InL xs
uncozip (InR xs) = map InR xs

-- * LSP helpers

-- | Detect what programming language the given file is written in.
detectLanguage :: (MonadSiskuApp m, MonadIO m) => FilePath -> m Text
detectLanguage filePath = do
  putTextLn $ toText $ "Detecting language for " <> filePath
  let ext = toText $ takeExtension filePath
  putTextLn $ "Looking for language for extension " <> ext
  lspSettings <- Map.elems <$> getLspSettingMap
  putTextLn $ "LspSettings: " <> show lspSettings
  let matches = mapMaybe ?? lspSettings $ \LspSetting {_language = language, _extensions = extensions} ->
        if ext `elem` extensions
          then Just language
          else Nothing
  putTextLn $ "Detected language: " <> show matches
  case matches of
    [] -> error $ "Could not detect language for " <> show filePath
    [language] -> pure language
    _ -> error $ "Multiple languages detected for " <> show filePath <> "\nTODO: Implement language detection when multiple languages are detected"

-- | Get the root path of the given file.
searchRootPath :: (MonadIO m, MonadSiskuApp m) => Text -> FilePath -> m String
searchRootPath language filePath = do
  lspSetting <- fromMaybe (error $ "Language " <> show language <> " not found") . view (at language) <$> getLspSettingMap
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
filterExcluded :: (MonadSiskuApp m, MonadIO m) => Text -> [FilePath] -> m [FilePath]
filterExcluded language sourceFiles = do
  lspSetting <- fromMaybe (error $ "Language " <> show language <> " not found") . view (at language) <$> getLspSettingMap
  excludedFiles <- liftIO $ traverse makeAbsolute . concat =<< traverse (glob . toString) (lspSetting ^. excludePatterns)
  pure $ filter (`notElem` excludedFiles) sourceFiles

-- | Get the command to run the Language Server.
getCommand :: MonadSiskuApp m => Text -> m String
getCommand language = do
  config <- getConfig
  let lspSetting = view (lspSettingMap . to unwrapLspSettingMap . at language) config
  case lspSetting of
    Nothing -> error $ "Language " <> show language <> " not found"
    Just lspSetting -> pure $ toString $ lspSetting ^. command