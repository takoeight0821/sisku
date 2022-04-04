module Sisku.Token where

import Data.Aeson
import qualified Data.Text as Text
import Relude
import Text.Parsec (anyChar, eof, manyTill, notFollowedBy, parse, satisfy, spaces, string, try)
import Text.Parsec.Text (Parser)
import Unicode.Char (isPunctuation, isSymbol, isXIDContinue, isXIDStart)

data Token
  = Ident {_identifier :: Text}
  | Symbol {_symbol :: Text}
  | Placeholder {_placeholder :: Text}
  | OtherChar {_char :: Char}
  deriving stock (Eq, Show, Generic)

instance ToJSON Token where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON Token where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

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
