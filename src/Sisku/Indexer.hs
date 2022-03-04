-- | The interface of Sisku indexer.
module Sisku.Indexer where

import Sisku.Hovercraft

class Indexer m where
  -- | Build an index
  build :: m Hovercraft