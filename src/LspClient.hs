{-# LANGUAGE OverloadedStrings #-}

module LspClient where

import Control.Applicative.Combinators
import Control.Lens ((^.), view)
import Debug.Trace (traceIO)
import Language.LSP.Test
import Language.LSP.Types (List (List))
import Language.LSP.Types.Lens (HasChildren (children), HasSelectionRange (selectionRange), HasStart (start), HasName (name))
import Relude
import System.Process

runLspClient :: FilePath -> String -> String -> [String] -> IO ()
runLspClient path file cmd args = do
  (Just hin, Just hout, _, _) <- createProcess (proc cmd args) {std_in = CreatePipe, std_out = CreatePipe}
  hSetBuffering hin NoBuffering
  hSetBuffering hout NoBuffering
  runSessionWithHandles hin hout defaultConfig fullCaps path $ do
    doc <- openDoc file "haskell"
    liftIO $ traceIO $ show doc
    skipMany anyNotification
    getDocumentSymbols doc >>= \case
      Right _ -> pure ()
      Left docSymbols -> printAllHovers doc docSymbols
  where
    printAllHovers doc docSymbols = do
      liftIO $ traceIO $ show (map (view name) docSymbols)
      traverse_ ?? docSymbols $ \docSymbol -> do
        let pos = docSymbol ^. selectionRange . start
        hover <- getHover doc pos
        print $ docSymbol ^. name
        print hover
        case docSymbol ^. children of
          Nothing -> pure ()
          Just (List cs) -> printAllHovers doc cs

