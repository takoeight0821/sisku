{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Sisku (loadLsifFromFile, indexToHovercraft, buildHovercraft, Hovercraft (..), SearchApi, searchServer, filterByQuery) where

import Control.Applicative.Combinators
import Control.Lens ((^.))
import Data.Aeson
import qualified Data.Text as Text
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

-- * Build hovercrafts via LSP

buildHovercraft :: FilePath -> String -> String -> [String] -> IO [Hovercraft]
buildHovercraft path ext cmd args = do
  files <- filter (ext `isExtensionOf`) <$> listFilesRecursive path
  (Just hin, Just hout, _, _) <- createProcess (proc cmd args) {std_in = CreatePipe, std_out = CreatePipe}
  hSetBuffering hin NoBuffering
  hSetBuffering hout NoBuffering
  let config = defaultConfig {messageTimeout = 120}
  runSessionWithHandles hin hout config fullCaps path $ do
    fmap concat $
      traverse ?? files $ \file -> do
        doc <- openDoc (makeRelative path file) "haskell"
        skipMany anyNotification
        hovercrafts <-
          getDocumentSymbols doc >>= \case
            Right _ -> error "not implemented"
            Left docSymbols -> collectAllHovers doc docSymbols
        closeDoc doc
        pure hovercrafts
  where
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
