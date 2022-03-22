module Sisku.Search (search) where

import Control.Lens (view)
import Data.Foldable (foldl, minimum)
import Relude
import qualified Relude.Unsafe as Unsafe
import Sisku.Hovercraft hiding (entries)
import Sisku.Indexer.ExtractCodeBlock (tokenize)

search :: Text -> [Entry] -> Text -> [Entry]
search placeholderText entries query =
  take 100 $ sortOn ((minimum . (maxBound :) . map (levenshtein ?? tokenize placeholderText query)) . view signatureToken) entries

-- from https://rosettacode.org/wiki/Levenshtein_distance#Haskell
-- TODO: make more readable
levenshtein :: Eq a => [a] -> [a] -> Int
levenshtein s1 s2 = Unsafe.last $ foldl transform [0 .. length s1] s2
  where
    transform ns@(n : ns1) c = scanl calc (n + 1) $ zip3 s1 ns ns1
      where
        calc z (c1, x, y) = minimum [y + 1, z + 1, x + fromEnum (c1 /= c)]
    transform [] _ = error "unreachable"
