module Sisku.Indexer.ExtractCodeBlock where

import Control.Lens ((^.))
import Data.Aeson
import qualified Data.Text as Text
import Language.LSP.Types
import Language.LSP.Types.Lens (HasContents (contents), HasValue (value))
import Relude
import Sisku.Hovercraft
import Sisku.Indexer
import Text.Parsec (anyChar, eof, manyTill, parse, satisfy, spaces)
import Text.Parsec.Text (Parser)
import Unicode.Char (isPunctuation, isSymbol, isXIDContinue, isXIDStart)

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
        traceShowM $ tokenize sigText
        _otherValues <- pure $ Object (fromList [("signature", toJSON (tokenize sigText))]) : _otherValues
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

data Token
  = Ident {_identifier :: Text}
  | Symbol {_symbol :: Text}
  | OtherChar {_char :: Char}
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON Token

instance FromJSON Token

tokenize :: Text -> [Token]
tokenize input = case parse
  ( do
      spaces
      manyTill
        ( do
            x <- pIdent <|> pSymbol <|> pOtherChar
            spaces
            pure x
        )
        eof
  )
  ""
  input of
  Left err -> error $ show err
  Right x -> x

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
