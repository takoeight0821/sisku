{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Sisku (loadLsifFromFile, indexToHovercraft, buildHovercraft, Hovercraft (..), SearchApi, searchServer, filterByQuery) where

import Control.Applicative.Combinators
import Control.Lens ((^.), (^?))
import Data.Aeson
import Data.Aeson.Lens
import Data.Graph.Inductive (Gr, Graph (labNodes), Node, lab, mkGraph, pre, suc)
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
import Relude
import qualified Relude.Unsafe as Unsafe
import Servant.API
import Servant.Server
import System.Directory.Extra (listFilesRecursive)
import System.FilePath (isExtensionOf, makeRelative)
import System.Process

-- | LSIF Graph
newtype Index = Index
  { graph :: Gr Value Text
  }
  deriving stock (Show)

loadLsifFromFile :: FilePath -> IO Index
loadLsifFromFile filePath = do
  ls <- lines <$> readFileText filePath
  let index = valuesToIndex $ mapMaybe (decode . encodeUtf8) ls
  pure index

valuesToIndex :: [Value] -> Index
valuesToIndex vs = Index {graph = mkGraph nodes edges}
  where
    nodes =
      filter (\x -> x ^? key "type" == Just (String "vertex")) vs
        |> map (\x -> (Unsafe.fromJust $ fmap fromInteger $ x ^? key "id" . _Integer, x))
    edges =
      filter (\x -> x ^? key "type" == Just (String "edge")) vs
        |> concatMap convEdge
    convEdge x =
      let inVs :: [Int] = case (x ^? key "inVs" . _Array, x ^? key "inV") of
            (Just inVs', Nothing) -> mapMaybe (fmap fromInteger . (^? _Integer)) $ toList inVs'
            (Nothing, Just inV) -> toList $ fmap fromInteger $ inV ^? _Integer
            _ -> error "invalid"
          outV :: Int = Unsafe.fromJust $ fmap fromInteger (x ^? key "outV" . _Integer)
          label = fromMaybe "noLabel" (x ^? key "label" . _String)
       in map (outV,,label) inVs

indexToHovercraft :: Index -> [Hovercraft]
indexToHovercraft Index {graph = gr} =
  map ?? hoverResults $ \hoverResult ->
    executingState Hovercraft {_hover = nodeToHover hoverResult, _definitions = [], _moniker = Null} do
      traverse_ goResult (concatMap results (pre gr hoverResult))
  where
    getValue x = fromMaybe Null $ lab gr x
    nodeToHover x = case fromMaybe Null (lab gr x) ^? key "result" of
      Just hover -> case fromJSON hover of
        Success hover' -> hover'
        Error mes -> error $ toText mes
      Nothing -> error "Invalid hover"
    getRange :: Node -> Range
    getRange x = case fromJSON $ fromMaybe Null $ lab gr x of
      Success hr -> hr
      Error mes -> error $ toText mes

    hoverResults = map fst $ filter (\(_, v) -> v ^? key "label" == Just (String "hoverResult")) $ labNodes gr
    results i = concatMap ?? suc gr i $ \next ->
      if getValue next ^? key "label" == Just (String "resultSet")
        then suc gr next
        else [next]

    defLoc defNode = do
      rangeNode <- suc gr defNode
      documentNode <- filter (\p -> Unsafe.fromJust (lab gr p) ^? key "label" == Just (String "document")) (pre gr rangeNode)
      let uri = case fromJSON $ Unsafe.fromJust $ getValue documentNode ^? key "uri" of
            Success x -> x
            Error mes -> error $ toText mes
      pure $ InL $ Location {_uri = uri, _range = getRange rangeNode}

    goResult r =
      case getValue r ^? key "label" of
        Just (String "definitionResult") -> do
          modify $ \x -> x {_definitions = _definitions x <> defLoc r}
        Just (String "moniker") -> do
          modify $ \x -> x {_moniker = getValue r}
        _ -> pure ()

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
