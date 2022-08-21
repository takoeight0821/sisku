{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Sisku.Indexer.FilterEmpty where

import Control.Lens (view)
import qualified Data.Text as Text
import Language.LSP.Types (Hover (..), HoverContents (..), List (..), MarkupContent (..))
import Language.LSP.Types.Lens (HasHover (hover))
import Relude
import Sisku.Hovercraft

filterEmpty :: Hovercraft -> Hovercraft
filterEmpty (Hovercraft projectId pages) =
  Hovercraft projectId $
    filter (not . null . view entries) $
      map (\(Page entries) -> Page $ filter (not . isEmpty . view hover) entries) pages
  where
    isEmpty :: Hover -> Bool
    isEmpty (Hover {_contents = HoverContentsMS (List [])}) = True
    isEmpty (Hover {_contents = HoverContentsMS (List _)}) = False
    isEmpty (Hover {_contents = HoverContents (MarkupContent _ txt)})
      | Text.null txt = True
      | otherwise = False