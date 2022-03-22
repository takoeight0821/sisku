module Sisku.Indexer.ExtractCodeBlock where

import Control.Lens ((^.))
import qualified Data.Text as Text
import Language.LSP.Types
import Language.LSP.Types.Lens (HasContents (contents), HasValue (value))
import Relude
import Sisku.Hovercraft
import Sisku.Indexer
import Text.Parsec (anyChar, eof, manyTill, notFollowedBy, parse, satisfy, spaces, string, try)
import Text.Parsec.Text (Parser)
import Unicode.Char (isPunctuation, isSymbol, isXIDContinue, isXIDStart)

extractCodeBlock :: LanguageClient -> LanguageClient
extractCodeBlock = onDecorate $ \super Entry {..} -> do
  let contentsLines = case _hover ^. contents of
        HoverContents c -> lines $ c ^. value
        _ -> ["HoverContentsMS"]
  case parseAndExtract contentsLines of
    [] -> super Entry {..}
    sigTexts -> do
      let _signatureToken = map (tokenize "_") sigTexts
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

tokenize :: Text -> Text -> [Token]
tokenize placeholder input = case parse
  ( do
      spaces
      manyTill
        ( do
            x <- try (pPlaceholder placeholder) <|> pIdent <|> pSymbol <|> pOtherChar
            spaces
            pure x
        )
        eof
  )
  ""
  input of
  Left err -> error $ show err
  Right x -> x

pPlaceholder :: Text -> Parser Token
pPlaceholder placeholderText = do
  _ <- string (toString placeholderText)
  notFollowedBy (pIdent <|> pSymbol)
  pure $ Placeholder placeholderText

pIdent :: Parser Token
pIdent = do
  start <- satisfy isXIDStart
  continue <- many (satisfy isXIDContinue)
  pure $ Ident (fromString $ start : continue)

pSymbol :: Parser Token
pSymbol = do
  (satisfy isPunctuation >>= \x -> pure (Symbol (Text.singleton x)))
    <|> (some (satisfy isSymbol) >>= \x -> pure (Symbol (fromString x)))

pOtherChar :: Parser Token
pOtherChar = do
  OtherChar <$> anyChar
