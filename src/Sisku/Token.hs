{-# OPTIONS_GHC -Wno-orphans #-}

module Sisku.Token where

import Codec.Serialise
import Data.Aeson
import qualified Data.Text as Text
import Relude
import Text.Megaparsec (MonadParsec, Pos, SourcePos, anySingle, eof, getOffset, getSourcePos, manyTill, notFollowedBy, parse, satisfy, try)
import Text.Megaparsec.Char (space, string)
import Unicode.Char (isPunctuation, isSymbol, isXIDContinue, isXIDStart)

data Token
  = Ident {_identifier :: Text}
  | Symbol {_symbol :: Text}
  | Placeholder {_placeholder :: Text}
  | OtherChar {_char :: Char}
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON Token where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON Token where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

instance Serialise Token

data WithPos a = WithPos
  { _startPos :: SourcePos,
    _endPos :: SourcePos,
    _length :: Int,
    _value :: a
  }
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON Pos

instance ToJSON SourcePos

instance ToJSON a => ToJSON (WithPos a) where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON Pos

instance FromJSON SourcePos

instance FromJSON a => FromJSON (WithPos a) where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

instance Serialise Pos

instance Serialise SourcePos

instance Serialise a => Serialise (WithPos a)

tokenize :: Text -> Text -> [WithPos Token]
tokenize placeholder input = case parse
  ( do
      space
      manyTill
        ( try (pPlaceholder placeholder) <|> pIdent <|> pSymbol <|> pOtherChar
        )
        eof
  )
  ""
  input of
  Left err -> error $ show err
  Right x -> x

pPlaceholder :: MonadParsec Void Text m => Text -> m (WithPos Token)
pPlaceholder placeholderText = do
  startPos <- getSourcePos
  startOffset <- getOffset
  _ <- string placeholderText
  notFollowedBy (pIdent <|> pSymbol)
  space
  endPos <- getSourcePos
  endOffset <- getOffset
  pure $ WithPos startPos endPos (endOffset - startOffset) $ Placeholder placeholderText

pIdent :: MonadParsec Void Text m => m (WithPos Token)
pIdent = do
  startPos <- getSourcePos
  startOffset <- getOffset
  start <- satisfy isXIDStart
  continue <- many (satisfy isXIDContinue)
  space
  endOffset <- getOffset
  endPos <- getSourcePos
  pure $ WithPos startPos endPos (endOffset - startOffset) $ Ident (fromString $ start : continue)

pSymbol :: MonadParsec Void Text m => m (WithPos Token)
pSymbol = do
  startPos <- getSourcePos
  startOffset <- getOffset
  symbol <-
    (satisfy isPunctuation >>= \x -> pure (Symbol (Text.singleton x)))
      <|> (some (satisfy isSymbol) >>= \x -> pure (Symbol (fromString x)))
  space
  endPos <- getSourcePos
  endOffset <- getOffset
  pure $ WithPos startPos endPos (endOffset - startOffset) symbol

pOtherChar :: MonadParsec Void Text m => m (WithPos Token)
pOtherChar = do
  startPos <- getSourcePos
  startOffset <- getOffset
  char <- OtherChar <$> anySingle
  space
  endPos <- getSourcePos
  endOffset <- getOffset
  pure $ WithPos startPos endPos (endOffset - startOffset) char