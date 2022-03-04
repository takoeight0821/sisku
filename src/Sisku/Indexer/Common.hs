{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Sisku.Indexer.Common (CommonIndexer (..)) where

import Control.Lens (over, view, (^.))
import Control.Lens.TH
import Data.Aeson (Value (Null))
import Data.Traversable (for)
import Language.LSP.Test (Session, closeDoc, defaultConfig, fullCaps, openDoc, runSessionWithConfig)
import Language.LSP.Types (DocumentSymbol (..), List (List), SymbolInformation (..), TextDocumentIdentifier)
import Language.LSP.Types.Lens (HasCharacter (character), HasRange (range), HasStart (start))
import Relude
import Sisku.App
import Sisku.Config (HasProjectId (..))
import Sisku.Hovercraft
import Sisku.Indexer
import qualified Sisku.Lsp as Lsp
import System.Directory.Extra (makeAbsolute)
import System.FilePath (makeRelative, normalise, takeExtension)
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

newtype CommonIndexer a = CommonIndexer {unCommonIndexer :: FilePath -> SiskuApp a}

instance Indexer CommonIndexer where
  build lc = CommonIndexer (buildHovercraft <=< generateEnv lc)

-- * Build hovercrafts via LSP

-- | Build a hovercraft using LSP.
buildHovercraft :: MonadIO m => Env -> m Hovercraft
buildHovercraft env@Env {_languageClient = LanguageClient {..}} = do
  let config = defaultConfig
  hovercrafts <-
    liftIO $
      runSessionWithConfig config (env ^. command) fullCaps (env ^. rootPath) $ do
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

-- | Generate a `Env` from the given file path.
generateEnv :: (MonadSiskuApp m, MonadIO m) => LanguageClient -> FilePath -> m Env
generateEnv _languageClient entryFilePath = do
  entryFilePath <- liftIO $ makeAbsolute entryFilePath
  _language <- Lsp.detectLanguage entryFilePath
  _rootPath <- Lsp.searchRootPath _language entryFilePath
  _sourceFiles <- fmap normalise <$> liftIO (glob (_rootPath <> "/**/*" <> takeExtension entryFilePath))
  _sourceFiles <- Lsp.filterExcluded _language _sourceFiles
  _command <- Lsp.getCommand _language
  _projectId <- view projectId <$> getConfig
  pure Env {..}

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
        pure
          [ Entry
              { _document = doc,
                _projectId = env ^. projectId,
                _hover = hover,
                _definitions = map toDefinition $ Lsp.uncozip definitions,
                _moniker = Null,
                _rootPath = env ^. rootPath
              }
          ]
      (Just hover, Just (List cs)) -> do
        ( Entry
            { _document = doc,
              _projectId = env ^. projectId,
              _hover = hover,
              _definitions = map toDefinition $ Lsp.uncozip definitions,
              _moniker = Null,
              _rootPath = env ^. rootPath
            }
            :
          )
          <$> craft env doc cs

instance Craftable SymbolInformation where
  craft env@Env {_languageClient = LanguageClient {..}} doc SymbolInformation {..} = do
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
                _definitions = map toDefinition $ Lsp.uncozip definitions,
                _moniker = Null,
                _rootPath = env ^. rootPath
              }
          ]
