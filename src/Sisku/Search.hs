module Sisku.Search (search, SearchResult (..)) where

import Control.Lens (view)
import Data.Aeson
import Data.Foldable (foldl, minimum)
import GHC.Real (infinity)
import Relude
import qualified Relude.Unsafe as Unsafe
import Sisku.Hovercraft hiding (entries)
import Sisku.Token
import Text.PrettyPrint.HughesPJClass (Pretty (pPrint))
import qualified Text.PrettyPrint.HughesPJClass as Pretty

data SearchResult = SearchResult
  {hit :: Entry, score :: Double}
  deriving stock (Eq, Show, Generic)

instance ToJSON SearchResult

instance FromJSON SearchResult

instance Pretty SearchResult where
  pPrint (SearchResult hit _) =
    Pretty.sep
      [ pPrint hit,
        "_____"
      ]

search :: Text -> [Entry] -> Text -> [SearchResult]
search placeholderText entries query =
  sortOn score $
    map ?? entries $ \entry ->
      SearchResult
        { hit = entry,
          score =
            view signatureToken entry
              & map
                ( snd
                    >>> levenshtein (\a b -> tokenDiff (_value a) (_value b)) ?? tokenize placeholderText query
                )
              & (fromRational infinity :)
              & minimum
        }

-- from https://rosettacode.org/wiki/Levenshtein_distance#Haskell
-- TODO: make more readable
levenshtein :: (a -> b -> Double) -> [a] -> [b] -> Double
levenshtein diff s1 s2 = Unsafe.last $ foldl transform [0 .. genericLength s1] s2
  where
    transform ns@(n : ns1) c = scanl calc (n + 1) $ zip3 s1 ns ns1
      where
        calc z (c1, x, y) =
          minimum
            [ y + 1, -- deletion
              z + 1, -- insertion
              x + diff c1 c -- substitution
            ]
    transform [] _ = error "unreachable"

tokenDiff :: Token -> Token -> Double
tokenDiff Placeholder {} _ = 0
tokenDiff _ Placeholder {} = 0
tokenDiff (Ident a) (Ident b) =
  let a' = toString a
      b' = toString b
   in levenshtein (\c1 c2 -> if c1 == c2 then 0 else 1) a' b' / max (genericLength a') (genericLength b')
tokenDiff (Symbol a) (Symbol b) =
  let a' = toString a
      b' = toString b
   in levenshtein (\c1 c2 -> if c1 == c2 then 0 else 1) a' b' / max (genericLength a') (genericLength b')
tokenDiff (OtherChar c1) (OtherChar c2)
  | c1 == c2 = 0
  | otherwise = 1
tokenDiff _ _ = 1
