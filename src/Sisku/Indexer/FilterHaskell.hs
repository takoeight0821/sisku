{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Sisku.Indexer.FilterHaskell where

import Control.Lens ((.~), (^.))
import Control.Monad.Extra (concatMapM)
import Data.Text qualified as Text
import Language.LSP.Types (Hover (Hover), HoverContents (HoverContents), MarkupContent (MarkupContent), MarkupKind (MkMarkdown))
import Language.LSP.Types.Lens (HasHover (hover), HasRange (range), contents)
import Relude
import Sisku.Indexer

filterHaskell :: LanguageClient -> LanguageClient
filterHaskell = onDecorate \super entry -> do
  let h :: Hover = entry ^. hover
  case h ^. contents of
    HoverContents (MarkupContent MkMarkdown v) -> do
      let vs =
            Text.splitOn "* * *\n" v
              & removeUnderscore
              & removeDictionary
              & removeClass
      let hs = map (\v -> Hover (HoverContents (MarkupContent MkMarkdown v)) (h ^. range)) vs
      let entries = map (\h -> entry & hover .~ h) hs
      concatMapM super entries
    _ -> super entry

removeUnderscore :: [Text] -> [Text]
removeUnderscore = filter (not . Text.isInfixOf "```haskell\n_ ::")

removeDictionary :: [Text] -> [Text]
removeDictionary = filter (not . Text.isInfixOf "```haskell\n$")

removeClass :: [Text] -> [Text]
removeClass = filter (not . Text.isInfixOf "```haskell\nC:")

{-
  onGetHover $ \super doc pos -> do
  mhover <- super doc pos
  case mhover of
    Nothing -> pure Nothing
    Just hover -> do
      case hover ^. contents of
        HoverContents (MarkupContent MkMarkdown v) -> do
          let filtered = Text.unlines $ filter (not . Text.null) $ removeClassDict $ lines v
          pure $ Just $ hover & contents .~ HoverContents (MarkupContent MkMarkdown filtered)
        _ -> pure $ Just hover

removeClassDict :: [Text] -> [Text]
removeClassDict [] = []
removeClassDict ("```haskell" : rest) = removeClassDict' rest
removeClassDict ("* * *" : rest) = removeClassDict rest
removeClassDict (line : rest) = line : removeClassDict rest

removeClassDict' :: [Text] -> [Text]
removeClassDict' [] = []
removeClassDict' (firstLine : rest)
  | "$" `Text.isPrefixOf` firstLine = removeClassDict (Unsafe.tail $ dropWhile (/= "```") rest)
  | "_ ::" `Text.isPrefixOf` firstLine = removeClassDict (Unsafe.tail $ dropWhile (/= "```") rest)
  | otherwise = "```haskell" : firstLine : removeClassDict rest
-}
