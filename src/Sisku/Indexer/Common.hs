{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Sisku.Indexer.Common (CommonIndexer (..)) where

import Control.Lens (At (at), over, view, (.~), (^.), _Just)
import Control.Lens.TH
import qualified Data.Map as Map
import Data.Traversable (for)
import Language.LSP.Test (Session, closeDoc, defaultConfig, fullCaps, openDoc, runSessionWithConfig)
import Language.LSP.Types (DocumentSymbol (..), List (List), SymbolInformation (..), TextDocumentIdentifier)
import Language.LSP.Types.Lens (HasCharacter (character), HasCommand (command), HasRange (range), HasSemanticTokens (semanticTokens), HasStart (start), HasWorkspace (workspace))
import Relude
import Sisku.App
import Sisku.Config (HasExcludePatterns (excludePatterns), HasExtensions (extensions), HasProjectId (..), HasRootUriPatterns (rootUriPatterns), LspSetting)
import Sisku.Hovercraft
import Sisku.Indexer
import qualified Sisku.Lsp as Lsp
import System.Directory.Extra (doesFileExist, getCurrentDirectory, makeAbsolute)
import System.FilePath (isValid, joinPath, makeRelative, normalise, splitPath, (</>))
import System.FilePath.Glob (glob)

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

newtype CommonIndexer a = CommonIndexer {unCommonIndexer :: SiskuApp a}

instance Indexer CommonIndexer where
  build lc = CommonIndexer do
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
buildHovercraft env@Env {_languageClient = LanguageClient {..}} = do
  let config = defaultConfig
  hovercrafts <-
    liftIO $
      runSessionWithConfig config (env ^. command) (fullCaps & workspace . _Just . semanticTokens .~ Nothing) (env ^. rootPath) $ do
        fileList <- for (env ^. sourceFiles) $ \file -> do
          doc <- openDoc (makeRelative (env ^. rootPath) file) (env ^. language)
          putTextLn $ toText $ "Opened " <> file
          pure (file, doc)
        for fileList $ uncurry seekFile
  pure $ Hovercraft (env ^. projectId) hovercrafts
  where
    seekFile :: FilePath -> TextDocumentIdentifier -> Session Page
    seekFile file doc = do
      putTextLn $ toText $ "Seeking file " <> file
      symbols <- getDocumentSymbols doc
      putTextLn $ "Got symbols for " <> show file
      page <-
        Page
          <$> case symbols of
            Right symInfos -> craft env doc symInfos
            Left docSymbols -> craft env doc docSymbols
      closeDoc doc
      pure page

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

instance Craftable DocumentSymbol where
  craft env@Env {_languageClient = LanguageClient {..}} doc DocumentSymbol {..} = do
    let pos = over character (+ 1) $ _selectionRange ^. start
    mhover <- getHover doc pos
    definitions <- getDefinitions doc pos

    case (mhover, _children) of
      (Nothing, Nothing) -> pure []
      (Nothing, Just (List cs)) -> craft env doc cs
      (Just hover, Nothing) -> do
        let entry =
              Entry
                { _document = doc,
                  _projectId = env ^. projectId,
                  _hover = hover,
                  _definitions = map toDefinition $ Lsp.uncozip definitions,
                  _signatureToken = [],
                  _otherValues = [],
                  _rootPath = env ^. rootPath
                }
        entry <- decorate entry
        pure [entry]
      (Just hover, Just (List cs)) -> do
        let entry =
              Entry
                { _document = doc,
                  _projectId = env ^. projectId,
                  _hover = hover,
                  _definitions = map toDefinition $ Lsp.uncozip definitions,
                  _signatureToken = [],
                  _otherValues = [],
                  _rootPath = env ^. rootPath
                }
        entry <- decorate entry
        (entry :) <$> craft env doc cs

instance Craftable SymbolInformation where
  craft env@Env {_languageClient = LanguageClient {..}} doc SymbolInformation {..} = do
    let pos = _location ^. range . start
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
                  _otherValues = [],
                  _rootPath = env ^. rootPath
                }
        entry <- decorate entry
        pure [entry]
