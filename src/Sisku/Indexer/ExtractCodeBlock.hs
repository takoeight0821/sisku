module Sisku.Indexer.ExtractCodeBlock where

import Control.Lens ((^.))
import Data.Aeson
import qualified Data.Text as Text
import Language.LSP.Types
import Language.LSP.Types.Lens (HasContents (contents), HasValue (value))
import Relude
import Sisku.Hovercraft
import Sisku.Indexer

extractCodeBlock :: LanguageClient -> LanguageClient
extractCodeBlock = onGetOtherValue $ \super Entry {..} -> do
  let contentsLines = case _hover ^. contents of
        HoverContents c -> lines $ c ^. value
        _ -> ["HoverContentsMS"]
  case parseAndExtract contentsLines of
    sigText
      | sigText == "" -> super Entry {..}
      | otherwise -> do
        traceM $ toString sigText
        _otherValues <- pure $ Object (fromList [("signature", String sigText)]) : _otherValues
        super Entry {..}

parseAndExtract :: [Text] -> Text
parseAndExtract [] = ""
parseAndExtract (line : rest)
  | "```" `Text.isPrefixOf` line = extract rest
  | otherwise = parseAndExtract rest

extract :: [Text] -> Text
extract [] = error "invalid markdown"
extract (line : rest)
  | "```" `Text.isPrefixOf` line = parseAndExtract rest
  | otherwise = line <> "\n" <> extract rest
