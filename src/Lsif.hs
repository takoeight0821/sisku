{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | LSIF type definitions
module Lsif (Index, loadLsifFromFile, indexToHovercraft) where

import Control.Lens ((^?))
import Data.Aeson
import Data.Aeson.Lens
import Data.Graph.Inductive
import Flow ((|>))
import Hovercraft
import Language.LSP.Types
import Relude
import qualified Relude.Unsafe as Unsafe

-- * LSIF

newtype HoverResult = HoverResult
  { result :: Hover
  }
  deriving stock (Show, Generic)

instance ToJSON HoverResult

instance FromJSON HoverResult

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
    executingState
      Hovercraft
        { _hover = nodeToHover hoverResult,
          _definitions = [],
          _moniker = Null,
          _rootPath = "" -- FIXME: Add root path
        }
      do
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
