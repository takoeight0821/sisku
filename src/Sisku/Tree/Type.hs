module Sisku.Tree.Type where

import Codec.Serialise (Serialise)
import Data.Aeson
import Relude
import Sisku.Token

data Tree
  = Branch [Tree]
  | Leaf Token
  deriving stock (Eq, Show, Generic)

instance ToJSON Tree

instance FromJSON Tree

instance Serialise Tree
