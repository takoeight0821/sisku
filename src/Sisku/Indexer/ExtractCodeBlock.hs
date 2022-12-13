module Sisku.Indexer.ExtractCodeBlock where

import Control.Lens ((^.))
import Data.Text qualified as Text
import Language.LSP.Types
import Language.LSP.Types.Lens (HasContents (contents), HasValue (value))
import Relude
import Sisku.Hovercraft
import Sisku.Indexer
import Sisku.Token

extractCodeBlock :: LanguageClient -> LanguageClient
extractCodeBlock = onDecorate $ \super Entry {..} -> do
  let contentsLines = case _hover ^. contents of
        HoverContents c -> lines $ c ^. value
        _ -> ["HoverContentsMS"]
  case parseAndExtract contentsLines of
    [] -> super Entry {..}
    sigTexts -> do
      let _signatureToken = map (\sigText -> (sigText, tokenize "_" sigText)) sigTexts
      super Entry {..}

parseAndExtract :: [Text] -> [Text]
parseAndExtract [] = []
parseAndExtract (line : rest)
  | "```" `Text.isPrefixOf` line = extract "" rest
  | otherwise = parseAndExtract rest

extract :: Text -> [Text] -> [Text]
extract acc [] = [acc]
extract acc (line : rest)
  | "```" `Text.isPrefixOf` line = acc : parseAndExtract rest
  | otherwise = extract (acc <> line <> "\n") rest
