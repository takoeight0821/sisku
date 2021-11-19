{-# LANGUAGE TemplateHaskell #-}

module Sisku (loadFile, search, Hovercraft (..), SearchApi, searchServer, filterByQuery) where

import Control.Lens ((^.), (^?))
import Data.Aeson
import Data.Aeson.Lens
import Data.Graph.Inductive (Gr, Graph (labNodes), Node, lab, mkGraph, pre, suc)
import qualified Data.Text as Text
import Flow ((|>))
import Language.LSP.Types
import Language.LSP.Types.Lens (HasContents (contents), HasValue (value))
import Relude
import qualified Relude.Unsafe as Unsafe
import Servant.API
import Servant.Server

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

-- | Hover document and definition information
data Hovercraft = Hovercraft
  { hover :: Hover,
    definitions :: [Location],
    moniker :: Value
  }
  deriving stock (Show, Generic)

instance ToJSON Hovercraft

search :: Index -> (Hovercraft -> Bool) -> [Hovercraft]
search Index {graph = gr} filterPred = filter filterPred $
  map ?? hoverResults $ \hoverResult ->
    executingState Hovercraft {hover = getHover hoverResult, definitions = [], moniker = Null} do
      traverse_ goResult (concatMap results (pre gr hoverResult))
  where
    getValue x = fromMaybe Null $ lab gr x
    getHover x = case fromMaybe Null (lab gr x) ^? key "result" of
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
      pure $ Location {_uri = uri, _range = getRange rangeNode}

    goResult r =
      case getValue r ^? key "label" of
        Just (String "definitionResult") -> do
          modify $ \x -> x {definitions = definitions x <> defLoc r}
        Just (String "moniker") -> do
          modify $ \x -> x {moniker = getValue r}
        _ -> pure ()

-- * Server

type SearchApi =
  "all-list" :> Get '[JSON] [Hovercraft]
    :<|> "search" :> QueryParam "q" String :> Get '[JSON] [Hovercraft]

searchServer :: Index -> Server SearchApi
searchServer index = allListHandler :<|> searchHandler
  where
    allListHandler :: Handler [Hovercraft]
    allListHandler = return (search index (const True))

    searchHandler :: Maybe String -> Handler [Hovercraft]
    searchHandler Nothing = return []
    searchHandler (Just query) = return (search index (filterByQuery $ toText query))

filterByQuery :: Text -> Hovercraft -> Bool
filterByQuery q Hovercraft {..}
  | Text.null q = True
  | otherwise = hover ^. contents |> hoverContentsToString |> Text.isInfixOf q
  where
    hoverContentsToString (HoverContents markedContent) = markedContent ^. value
    hoverContentsToString _ = error "not implemented"
