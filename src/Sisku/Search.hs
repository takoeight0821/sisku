module Sisku.Search where

import Control.Lens ((^.), (^?))
import Data.Aeson (Result (Error, Success), fromJSON)
import Data.Aeson.Lens (key)
import Data.Foldable (foldl, minimum)
import Relude
import qualified Relude.Unsafe as Unsafe
import Sisku.Hovercraft hiding (entries)
import Sisku.Indexer.ExtractCodeBlock (Token, tokenize)

search :: [Entry] -> Text -> [Entry]
search entries query =
  take 100 $ sortOn ((levenshtein ?? tokenize query) . signature) entries

levenshtein :: Eq a => [a] -> [a] -> Int
levenshtein s1 s2 = Unsafe.last $ foldl transform [0 .. length s1] s2
  where
    transform ns@(n : ns1) c = scanl calc (n + 1) $ zip3 s1 ns ns1
      where
        calc z (c1, x, y) = minimum [y + 1, z + 1, x + fromEnum (c1 /= c)]
    transform [] _ = error "unreachable"

signature :: Entry -> [Token]
signature e = go $ e ^. otherValues
  where
    go [] = []
    go (x : rest) =
      fromMaybe
        []
        ( case fromJSON $ Unsafe.fromJust $ x ^? key "signature" of
            Error err -> error $ toText err
            Success v -> v
        )
        <> go rest
