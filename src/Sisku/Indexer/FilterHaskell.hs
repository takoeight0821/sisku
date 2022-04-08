module Sisku.Indexer.FilterHaskell where

import Control.Lens ((.~), (^.))
import qualified Data.Text as Text
import Language.LSP.Types (HoverContents (HoverContents), MarkupContent (MarkupContent), MarkupKind (MkMarkdown))
import Language.LSP.Types.Lens (contents)
import Relude
import qualified Relude.Unsafe as Unsafe
import Sisku.Indexer

filterHaskell :: LanguageClient -> LanguageClient
filterHaskell = onGetHover $ \super doc pos -> do
  mhover <- super doc pos
  case mhover of
    Nothing -> pure Nothing
    Just hover -> do
      case hover ^. contents of
        HoverContents (MarkupContent MkMarkdown v) -> do
          let filtered = Text.unlines $ removeClassDict $ lines v
          pure $ Just $ hover & contents .~ HoverContents (MarkupContent MkMarkdown filtered)
        _ -> pure $ Just hover

removeClassDict :: [Text] -> [Text]
removeClassDict [] = []
removeClassDict ("```haskell" : rest) = removeClassDict' rest
removeClassDict (line : rest) = line : removeClassDict rest

removeClassDict' :: [Text] -> [Text]
removeClassDict' [] = []
removeClassDict' (firstLine : rest)
  | "$d" `Text.isPrefixOf` firstLine = removeClassDict (Unsafe.tail $ dropWhile (/= "```") rest)
  | "_ ::" `Text.isPrefixOf` firstLine = removeClassDict (Unsafe.tail $ dropWhile (/= "```") rest)
  | otherwise = "```haskell" : firstLine : removeClassDict rest
