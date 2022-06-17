{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sisku.Indexer.Exhaustive (ExhaustiveIndexer (..)) where

import Control.Lens (At (at), iconcatMap, to, view, (.~), (^.), _Just)
import Control.Lens.TH
import Data.List.Extra (groupOn)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Traversable (for)
import Language.LSP.Test (Session, closeDoc, defaultConfig, fullCaps, openDoc, runSessionWithConfig)
import Language.LSP.Types (Position (Position), TextDocumentIdentifier, uriToFilePath)
import Language.LSP.Types.Lens (HasCommand (command), HasHover (hover), HasSemanticTokens (semanticTokens), HasUri (uri), HasWorkspace (workspace))
import Relude
import Sisku.App
import Sisku.Config (HasExcludePatterns (excludePatterns), HasExtensions (extensions), HasProjectId (..), HasRootUriPatterns (rootUriPatterns), LspSetting)
import Sisku.Hovercraft
import Sisku.Indexer
import qualified Sisku.Lsp as Lsp
import System.Directory.Extra (doesFileExist, getCurrentDirectory, makeAbsolute)
import System.FilePath (isValid, joinPath, makeRelative, normalise, splitPath, (</>))
import System.FilePath.Glob (glob)
import Witherable (hashNubOn)

data Env = Env
  { _command :: FilePath,
    _sourceFiles :: [FilePath],
    _rootPath :: FilePath,
    _language :: Text,
    _projectId :: Text,
    _languageClient :: LanguageClient
  }
  deriving stock (Generic)

makeFieldsNoPrefix ''Env

newtype ExhaustiveIndexer a = ExhaustiveIndexer {unExhaustiveIndexer :: SiskuApp a}

instance Indexer ExhaustiveIndexer where
  build lc = ExhaustiveIndexer do
    languages <- Map.keys <$> getLspSettingMap
    envs <- generateAllEnvs (map (,lc) languages)
    hs <- traverse (\(_, env) -> buildHovercraft env) envs
    pure $ mergeHovercraft hs
    where
      mergeHovercraft hs@(Hovercraft {_projectId} : _) = foldr merge Hovercraft {_projectId = _projectId, _pages = []} hs
      mergeHovercraft [] = error "mergeHovercraft: empty list"
      merge Hovercraft {_projectId, _pages = ps1} Hovercraft {_pages = ps2} = Hovercraft {_projectId, _pages = ps1 <> ps2}

-- * Build hovercrafts via LSP

-- | Build a hovercraft using LSP.
buildHovercraft :: MonadIO m => Env -> m Hovercraft
buildHovercraft env = do
  let config = defaultConfig
  hovercrafts <-
    liftIO $
      runSessionWithConfig config (env ^. command) (fullCaps & workspace . _Just . semanticTokens .~ Nothing) (env ^. rootPath) do
        for (env ^. sourceFiles) $ \file -> do
          doc <- openDoc (makeRelative (env ^. rootPath) file) (env ^. language)
          putTextLn $ toText $ "Opened " <> file
          seekFile file doc
  let hovercrafts' = hashNubOn (view (hover . to Lsp.hoverContents)) $ concat hovercrafts
  let hovercrafts'' = groupOn (view document) hovercrafts'
  pure $ Hovercraft (env ^. projectId) (map Page hovercrafts'')
  where
    seekFile :: FilePath -> TextDocumentIdentifier -> Session [Entry]
    seekFile file doc = do
      putTextLn $ toText $ "Seeking file " <> file
      positions <- getAllPositions doc
      entries <- craft env doc positions
      closeDoc doc
      pure entries

getAllPositions :: MonadIO m => TextDocumentIdentifier -> m [Position]
getAllPositions doc = do
  case doc ^. uri . to uriToFilePath of
    Nothing -> error $ "invalid URI: " <> show (doc ^. uri)
    Just filePath -> do
      fileContents <- lines <$> readFileText filePath
      pure $ iconcatMap (\i line -> [Position (fromIntegral i) (fromIntegral n) | n <- [0 .. Text.length line]]) fileContents

-- | Generate all type of 'Env' for given 'LanguageClient's.
generateAllEnvs :: (MonadSiskuApp m, MonadIO m) => [(Text, LanguageClient)] -> m [(Text, Env)]
generateAllEnvs lcs = do
  menvs <- for lcs \(lang, _languageClient) -> do
    let _language = lang
    lspSetting <- fromMaybe (error $ "Language " <> lang <> " not found") . view (at lang) <$> getLspSettingMap
    pwd <- liftIO getCurrentDirectory
    mRootPath <- findRootPath lang (lspSetting ^. rootUriPatterns) [pwd]
    case mRootPath of
      Nothing -> pure Nothing
      Just _rootPath -> do
        _sourceFiles <- findSourceFiles lspSetting _rootPath
        let _command = toString $ lspSetting ^. command
        _projectId <- view projectId <$> getConfig
        pure $ Just (lang, Env {..})
  pure $ catMaybes menvs
  where
    findRootPath _ _ [] = pure Nothing
    findRootPath lang patterns (p : ps) = do
      isRootPath <- anyM (\pattern -> liftIO $ doesFileExist $ p </> toString pattern) patterns
      if isRootPath
        then pure $ Just p
        else findRootPath lang patterns $ case joinPath <$> viaNonEmpty init (splitPath p) of
          Just parentPath | isValid parentPath && parentPath `notElem` ps -> parentPath : ps
          _ -> ps
    findSourceFiles :: (MonadSiskuApp m, MonadIO m) => LspSetting -> FilePath -> m [FilePath]
    findSourceFiles lspSetting rootPath = do
      let exts = lspSetting ^. extensions
      concat <$> for exts \ext -> do
        files <- fmap normalise <$> liftIO (glob (rootPath <> "/**/*" <> toString ext))
        excludedFiles <- liftIO $ traverse makeAbsolute . concat =<< traverse (glob . toString) (lspSetting ^. excludePatterns)
        pure $ filter (`notElem` excludedFiles) files

class Craftable a where
  craft :: Env -> TextDocumentIdentifier -> a -> Session [Entry]

instance Craftable a => Craftable [a] where
  craft env doc xs = concat <$> traverse (craft env doc) xs

instance Craftable Position where
  craft env@Env {_languageClient = LanguageClient {..}} doc pos = do
    mhover <- getHover doc pos
    definitions <- getDefinitions doc pos
    case mhover of
      Nothing -> pure []
      Just hover -> do
        let entry =
              Entry
                { _document = doc,
                  _projectId = env ^. projectId,
                  _hover = hover,
                  _definitions = map toDefinition $ Lsp.uncozip definitions,
                  _signatureToken = [],
                  _typeTree = [],
                  _rootPath = env ^. rootPath
                }
        decorate entry
