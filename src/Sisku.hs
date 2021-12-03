{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Sisku (loadLsifFromFile, indexToHovercraft, LspConfig (..), buildHovercraft, Hovercraft (..), SearchApi, searchServer, filterByQuery) where

import Control.Applicative.Combinators (skipMany, skipManyTill)
import Control.Lens ((^.))
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Traversable (for)
import Flow ((|>))
import Hovercraft
import Language.LSP.Test
import Language.LSP.Types
import Language.LSP.Types.Lens
  ( HasChildren (children),
    HasContents (contents),
    HasSelectionRange (selectionRange),
    HasStart (start),
    HasValue (value),
  )
import Lsif
import Relude
import Servant.API
import Servant.Server
import System.Directory.Extra (listFilesRecursive)
import System.FilePath (isExtensionOf, makeRelative)
import System.Process
  ( CreateProcess (std_in, std_out),
    StdStream (CreatePipe),
    createProcess,
    shell,
  )

data LspConfig = LspConfig
  { lspConfigCommand :: FilePath,
    lspConfigRootPath :: FilePath,
    lspConfigExtension :: String,
    lspConfigLanguage :: Text
  }

instance ToJSON LspConfig where
  toJSON LspConfig {lspConfigCommand, lspConfigRootPath, lspConfigExtension, lspConfigLanguage} =
    Aeson.object
      [ "command" Aeson..= lspConfigCommand,
        "rootPath" Aeson..= lspConfigRootPath,
        "extension" Aeson..= lspConfigExtension,
        "language" Aeson..= lspConfigLanguage
      ]

instance FromJSON LspConfig where
  parseJSON = Aeson.withObject "LspConfig" $ \o ->
    LspConfig
      <$> o Aeson..: "command"
      <*> o Aeson..: "rootPath"
      <*> o Aeson..: "extension"
      <*> o Aeson..: "language"

-- * Build hovercrafts via LSP

buildHovercraft :: LspConfig -> IO [Hovercraft]
buildHovercraft LspConfig {..} = do
  files <- filter (lspConfigExtension `isExtensionOf`) <$> listFilesRecursive lspConfigRootPath
  (Just hin, Just hout, _, _) <- createProcess (shell lspConfigCommand) {std_in = CreatePipe, std_out = CreatePipe}
  hSetBuffering hin NoBuffering
  hSetBuffering hout NoBuffering
  let config = defaultConfig
  hovercrafts <-
    runSessionWithHandles hin hout config fullCaps lspConfigRootPath $ do
      for files seekFile
  pure $ concat hovercrafts
  where
    seekFile :: FilePath -> Session [Hovercraft]
    seekFile file = do
      traceM $ "Seeking " <> show file
      doc <- openDoc (makeRelative lspConfigRootPath file) lspConfigLanguage
      traceM $ "Opened " <> show file
      -- wait until the server is ready
      traceM "Wait for diagnostics"
      -- TODO: This is a hack, we should make clear why we need to wait for diagnostics and haskell-language-server stops working.
      if lspConfigLanguage == "haskell"
        then pure () -- haskell-language-server hang up when we call waitForDiagnostics
        else void waitForDiagnostics
      hovercrafts <-
        getDocumentSymbols doc >>= \case
          Right _ -> error "not implemented"
          Left docSymbols -> collectAllHovers doc docSymbols
      closeDoc doc
      pure hovercrafts

    collectAllHovers doc docSymbols = concat <$> traverse (collectHover doc) docSymbols
    collectHover doc docSymbol = do
      let pos = docSymbol ^. selectionRange . start
      hover <- getHover doc pos
      definitions <- getDefinitions doc pos
      case (hover, docSymbol ^. children) of
        (Nothing, Nothing) -> pure []
        (Nothing, Just (List cs)) -> collectAllHovers doc cs
        (Just hover, Nothing) ->
          pure
            [ Hovercraft
                { _hover = hover,
                  _definitions = uncozip definitions,
                  _moniker = Null
                }
            ]
        (Just hover, Just (List cs)) ->
          ( Hovercraft
              { _hover = hover,
                _definitions = uncozip definitions,
                _moniker = Null
              }
              :
          )
            <$> collectAllHovers doc cs
    uncozip (InL xs) = map InL xs
    uncozip (InR xs) = map InR xs

-- * Server

type SearchApi =
  "all-list" :> Get '[JSON] [Hovercraft]
    :<|> "search" :> QueryParam "q" String :> Get '[JSON] [Hovercraft]

searchServer :: [Hovercraft] -> Server SearchApi
searchServer hovercrafts = allListHandler :<|> searchHandler
  where
    allListHandler :: Handler [Hovercraft]
    allListHandler = return hovercrafts

    searchHandler :: Maybe String -> Handler [Hovercraft]
    searchHandler Nothing = return []
    searchHandler (Just query) = return (filter (filterByQuery $ toText query) hovercrafts)

filterByQuery :: Text -> Hovercraft -> Bool
filterByQuery q Hovercraft {_hover}
  | Text.null q = True
  | otherwise = _hover ^. contents |> hoverContentsToString |> Text.isInfixOf q
  where
    hoverContentsToString (HoverContents markedContent) = markedContent ^. value
    hoverContentsToString _ = error "not implemented"
