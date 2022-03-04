module Sisku.Indexer.MiddlewareExample where

import Flow ((|>))
import Relude (putTextLn)
import Sisku.Indexer (LanguageClient, defaultLanguageClient, onGetHover)
import Sisku.Indexer.Common (CommonIndexer)

type ExampleIndexer a = CommonIndexer

exampleLanguageClient :: LanguageClient
exampleLanguageClient =
  defaultLanguageClient
    |> onGetHover
      ( \super doc pos -> do
          putTextLn "onGetHover"
          super doc pos
      )