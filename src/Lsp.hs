{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lsp (buildHovercraft, LspConfig(..)) where

import Relude
import Data.Aeson
import Hovercraft
import Language.LSP.Test
import Language.LSP.Types
import System.Process
import System.FilePath.Glob
import Data.Traversable
import System.FilePath
import Control.Lens hiding (children, List, (.=))
import Language.LSP.Types.Lens

data LspConfig = LspConfig
  { lspConfigCommand :: FilePath,
    lspConfigSourceFilePatterns :: [String],
    lspConfigRootPath :: FilePath,
    lspConfigLanguage :: Text
  }

instance ToJSON LspConfig where
  toJSON LspConfig {..} =
    object
      [ "command" .= lspConfigCommand,
        "sourceFilePatterns" .= lspConfigSourceFilePatterns,
        "rootPath" .= lspConfigRootPath,
        "language" .= lspConfigLanguage
      ]

instance FromJSON LspConfig where
  parseJSON = withObject "LspConfig" $ \o ->
    LspConfig
      <$> o .: "command"
      <*> o .: "sourceFilePatterns"
      <*> o .: "rootPath"
      <*> o .: "language"

-- * Build hovercrafts via LSP

buildHovercraft :: LspConfig -> IO [Hovercraft]
buildHovercraft LspConfig {..} = do
  files <- concat <$> traverse glob lspConfigSourceFilePatterns
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
          Right symbolInformations -> collectAllHovers' doc (map (view location) symbolInformations)
          Left docSymbols -> collectAllHovers doc docSymbols
      closeDoc doc
      pure hovercrafts

    collectAllHovers doc docSymbols = concat <$> traverse (collectHover doc) docSymbols
    collectAllHovers' doc docSymbols = concat <$> traverse (collectHover' doc) docSymbols
    collectHover doc docSymbol = do
      let pos = docSymbol ^. selectionRange . start
      -- let pos = docSymbol ^. range . start
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
    collectHover' doc docSymbol = do
      let pos = docSymbol ^. range . start
      hover <- getHover doc pos
      definitions <- getDefinitions doc pos
      case hover of
        Nothing -> pure []
        Just hover ->
          pure
            [ Hovercraft
                { _hover = hover,
                  _definitions = uncozip definitions,
                  _moniker = Null
                }
            ]
    uncozip (InL xs) = map InL xs
    uncozip (InR xs) = map InR xs