{-# LANGUAGE OverloadedStrings #-}

module LspClient where

import Control.Applicative.Combinators
import Control.Lens ((^.))
import Language.LSP.Test
import Language.LSP.Types (List (List))
import Language.LSP.Types.Lens (HasChildren (children), HasSelectionRange (selectionRange), HasStart (start))
import Relude
import System.Directory.Extra (listFilesRecursive)
import System.FilePath (isExtensionOf, makeRelative)
import System.Process
import Data.Aeson (encode)

runLspClient :: FilePath -> String -> String -> [String] -> IO ()
runLspClient path ext cmd args = do
  files <- filter (ext `isExtensionOf`) <$> listFilesRecursive path
  (Just hin, Just hout, _, _) <- createProcess (proc cmd args) {std_in = CreatePipe, std_out = CreatePipe}
  hSetBuffering hin NoBuffering
  hSetBuffering hout NoBuffering
  runSessionWithHandles hin hout defaultConfig fullCaps path $ do
    hovers <- traverse ?? files $ \file -> do
      doc <- openDoc (makeRelative path file) "haskell"
      skipMany anyNotification
      hovers <-
        getDocumentSymbols doc >>= \case
          Right _ -> pure []
          Left docSymbols -> fmap (doc,) <$> collectAllHovers doc docSymbols
      closeDoc doc
      pure hovers
    putLBS $ encode hovers
  where
    collectAllHovers doc docSymbols = concat <$> traverse (collectHover doc) docSymbols
    collectHover doc docSymbol = do
      let pos = docSymbol ^. selectionRange . start
      hover <- getHover doc pos
      case docSymbol ^. children of
        Nothing -> pure $ toList hover
        Just (List cs) -> (toList hover <>) <$> collectAllHovers doc cs

