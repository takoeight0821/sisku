{-# OPTIONS_GHC -Wno-orphans #-}

module Sisku.Token where

import Codec.Serialise
import Data.Aeson
import qualified Data.Text as Text
import Relude
import Text.Megaparsec (MonadParsec, Pos, SourcePos (sourceColumn), anySingle, eof, getSourcePos, manyTill, notFollowedBy, parse, satisfy, try, unPos)
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

pPlaceholder :: MonadParsec Void Text m => Text -> m (WithPos Token)
pPlaceholder placeholderText = do
  start <- getSourcePos
  _ <- string placeholderText
  notFollowedBy (pIdent <|> pSymbol)
  end <- getSourcePos
  pure $ at start end $ Placeholder placeholderText

pIdent :: MonadParsec Void Text m => m (WithPos Token)
pIdent = do
  startPos <- getSourcePos
  start <- satisfy isXIDStart
  continue <- many (satisfy isXIDContinue)
  endPos <- getSourcePos
  pure $ at startPos endPos $ Ident (fromString $ start : continue)

pSymbol :: MonadParsec Void Text m => m (WithPos Token)
pSymbol = do
  startPos <- getSourcePos
  symbol <-
    (satisfy isPunctuation >>= \x -> pure (Symbol (Text.singleton x)))
      <|> (some (satisfy isSymbol) >>= \x -> pure (Symbol (fromString x)))
  endPos <- getSourcePos
  pure $ at startPos endPos symbol

pOtherChar :: MonadParsec Void Text m => m (WithPos Token)
pOtherChar = do
  startPos <- getSourcePos
  char <- OtherChar <$> anySingle
  endPos <- getSourcePos
  pure $ at startPos endPos char

at :: SourcePos -> SourcePos -> Token -> WithPos Token
at start end = WithPos start end (unPos (sourceColumn end) - unPos (sourceColumn start))