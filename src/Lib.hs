{-# LANGUAGE TemplateHaskell #-}

module Lib (loadFile, search, SearchResult(..), SearchApi, searchServer) where

import Control.Lens ((^?))
import Data.Aeson
import Data.Aeson.Lens
import Data.Graph.Inductive (Gr, Graph (labNodes), lab, mkGraph, pre, suc)
import Flow ((|>))
import Relude
import qualified Relude.Unsafe as Unsafe
import Servant.API
import Servant.Server
import Data.List (isInfixOf)

-- | LSIF Graph
newtype Index = Index
  { graph :: Gr Value Text
  }
  deriving stock (Show)

loadFile :: FilePath -> IO Index
loadFile filePath = do
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

-- | Search result
data SearchResult = SearchResult
  { hover :: Value,
    definition :: Value,
    defRanges :: [(Value, Value)],
    moniker :: Value
  }
  deriving stock (Show, Generic)

instance ToJSON SearchResult

search :: Index -> (SearchResult -> Bool) -> [SearchResult]
search Index {graph = gr} filterPred = filter filterPred $
  map ?? hoverResults $ \hoverResult ->
    executingState SearchResult {hover = getValue hoverResult, definition = Null, defRanges = [], moniker = Null} do
      traverse_ goResult (concatMap results (pre gr hoverResult))
  where
    getValue x = fromMaybe Null $ lab gr x

    hoverResults = map fst $ filter (\(_, v) -> v ^? key "label" == Just (String "hoverResult")) $ labNodes gr
    results i = concatMap ?? suc gr i $ \next ->
      if getValue next ^? key "label" == Just (String "resultSet")
        then suc gr next
        else [next]
    defRange defNode = do
      range <- suc gr defNode
      definition <- filter (\p -> Unsafe.fromJust (lab gr p) ^? key "label" == Just (String "document")) $ pre gr range
      pure (getValue definition, getValue range)

    goResult r =
      case getValue r ^? key "label" of
        Just (String "definitionResult") -> do
          modify $ \x -> x {definition = getValue r}
          modify $ \x -> x {defRanges = defRanges x <> defRange r}
        Just (String "moniker") -> do
          modify $ \x -> x {moniker = getValue r}
        _ -> pure ()

-- * Server

type SearchApi = "all-list" :> Get '[JSON] [SearchResult]
              :<|> "search" :> QueryParam "q" String :> Get '[JSON] [SearchResult]

searchServer :: Index -> Server SearchApi
searchServer index = allListHandler :<|> searchHandler
  where
    allListHandler :: Handler [SearchResult]
    allListHandler = return (search index (const True))

    searchHandler :: Maybe String -> Handler [SearchResult]
    searchHandler Nothing = return []
    searchHandler (Just query) = return (search index (filterByQuery query))

    filterByQuery [] _ = True
    filterByQuery q SearchResult {..} =
      case hover ^? key "result" . key "contents" . key "value" of
        Just (String contents) -> q `isInfixOf` toString contents
        _ -> False

