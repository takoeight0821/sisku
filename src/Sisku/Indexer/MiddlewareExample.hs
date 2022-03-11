module Sisku.Indexer.MiddlewareExample where

import Control.Lens ((^.))
import Flow ((|>))
import Language.LSP.Types (HoverContents (HoverContents))
import Language.LSP.Types.Lens (HasContents (contents), HasValue (value))
import Relude
import Sisku.Indexer (LanguageClient, defaultLanguageClient, onGetDocumentSymbols, onGetHover)
import Sisku.Indexer.Common (CommonIndexer)

type ExampleIndexer a = CommonIndexer

exampleLanguageClient :: LanguageClient
exampleLanguageClient =
  defaultLanguageClient
    |> onGetHover
      ( \super doc pos -> do
          putTextLn "onGetHover"
          mhover <- super doc pos
          case mhover of
            Nothing -> pure Nothing
            Just hover -> do
              let contentsLines = case hover ^. contents of
                    HoverContents c -> lines $ c ^. value
                    _ -> ["HoverContentsMS"]
              traceShowM contentsLines
              pure $ Just hover
      )
    |> onGetDocumentSymbols
      ( \super doc -> do
          putTextLn "onGetDocumentSymbols"
          a <- super doc
          putTextLn "onGetDocumentSymbols"
          pure a
      )