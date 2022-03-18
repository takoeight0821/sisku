-- | The interface of Sisku indexer.
module Sisku.Indexer where

import Data.Aeson (Value)
import Language.LSP.Test (Session)
import Language.LSP.Types (DocumentSymbol, Hover, Location, LocationLink, Position, SymbolInformation, TextDocumentIdentifier, type (|?))
import Relude
import Sisku.Hovercraft
import qualified Sisku.Lsp as Lsp

class Indexer m where
  -- | Build an index
  build :: LanguageClient -> m Hovercraft

data LanguageClient = LanguageClient
  { getDocumentSymbols :: TextDocumentIdentifier -> Session (Either [DocumentSymbol] [SymbolInformation]),
    getHover :: TextDocumentIdentifier -> Position -> Session (Maybe Hover),
    getDefinitions :: TextDocumentIdentifier -> Position -> Session ([Location] |? [LocationLink]),
    decorate :: Entry -> Session Entry
  }

defaultLanguageClient :: LanguageClient
defaultLanguageClient =
  LanguageClient
    { getDocumentSymbols = Lsp.getDocumentSymbols,
      getHover = Lsp.getHover,
      getDefinitions = Lsp.getDefinitions,
      decorate = pure
    }

onGetDocumentSymbols ::
  ( ( TextDocumentIdentifier ->
      Session (Either [DocumentSymbol] [SymbolInformation])
    ) ->
    TextDocumentIdentifier ->
    Session (Either [DocumentSymbol] [SymbolInformation])
  ) ->
  LanguageClient ->
  LanguageClient
onGetDocumentSymbols f lc = lc {getDocumentSymbols = f (getDocumentSymbols lc)}

onGetHover ::
  ( ( TextDocumentIdentifier ->
      Position ->
      Session (Maybe Hover)
    ) ->
    TextDocumentIdentifier ->
    Position ->
    Session (Maybe Hover)
  ) ->
  LanguageClient ->
  LanguageClient
onGetHover f lc = lc {getHover = f (getHover lc)}

onGetDefinitions ::
  ( ( TextDocumentIdentifier ->
      Position ->
      Session ([Location] |? [LocationLink])
    ) ->
    TextDocumentIdentifier ->
    Position ->
    Session ([Location] |? [LocationLink])
  ) ->
  LanguageClient ->
  LanguageClient
onGetDefinitions f lc = lc {getDefinitions = f (getDefinitions lc)}

onDecorate :: ((Entry -> Session Entry) -> Entry -> Session Entry) -> LanguageClient -> LanguageClient
onDecorate f lc = lc {decorate = f (decorate lc)}