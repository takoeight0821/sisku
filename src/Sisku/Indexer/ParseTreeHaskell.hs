module Sisku.Indexer.ParseTreeHaskell (parseTreeHaskell) where

import Relude
import Sisku.Hovercraft
import Sisku.Indexer
import Sisku.Token
import Sisku.Tree.Type

parseTreeHaskell :: LanguageClient -> LanguageClient
parseTreeHaskell = onDecorate $ \super Entry {..} -> do
  for_ _signatureToken \tokens -> do
    traceShowM tokens
  super Entry {..}
