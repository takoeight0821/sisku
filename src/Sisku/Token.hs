module Sisku.Token where

import Codec.Serialise
import Data.Aeson
import qualified Data.Text as Text
import Relude
import Text.Megaparsec (MonadParsec, anySingle, eof, manyTill, notFollowedBy, parse, satisfy, skipMany, try)
import Text.Megaparsec.Char (space, string)
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

instance Serialise Token

tokenize :: Text -> Text -> [Token]
tokenize placeholder input = case parse
  ( do
      space
      manyTill
        ( do
            x <- try (pPlaceholder placeholder) <|> pIdent <|> pSymbol <|> pOtherChar
            space
            pure x
        )
        eof
  )
  ""
  input of
  Left err -> error $ show err
  Right x -> x

pPlaceholder :: MonadParsec Void Text m => Text -> m Token
pPlaceholder placeholderText = do
  _ <- string placeholderText
  notFollowedBy (pIdent <|> pSymbol)
  pure $ Placeholder placeholderText

pIdent :: MonadParsec Void Text m => m Token
pIdent = do
  start <- satisfy isXIDStart
  continue <- many (satisfy isXIDContinue)
  pure $ Ident (fromString $ start : continue)

pSymbol :: MonadParsec Void Text m => m Token
pSymbol = do
  (satisfy isPunctuation >>= \x -> pure (Symbol (Text.singleton x)))
    <|> (some (satisfy isSymbol) >>= \x -> pure (Symbol (fromString x)))

pOtherChar :: MonadParsec Void Text m => m Token
pOtherChar = do
  OtherChar <$> anySingle
