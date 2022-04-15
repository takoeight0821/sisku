{-# LANGUAGE OverloadedStrings #-}

module Sisku.Commands.Search (parser) where

import Options.Applicative
import Relude
import qualified Sisku.Search as Search
import Sisku.Server (getAllHovercrafts, toEntries)
import Text.PrettyPrint.HughesPJClass (pPrint)

newtype Options = Options
  { query :: Text
  }

cmd :: Options -> IO ()
cmd Options {..} = do
  hs <- toEntries <$> getAllHovercrafts
  let results =
        Search.search "_" hs query
          & take 10
  mapM_ (print . pPrint) results

opts :: Parser Options
opts =
  Options
    <$> strArgument (help "search text")

parser :: Mod CommandFields (IO ())
parser = command "search" (info (cmd <$> opts) (progDesc "Search indexed documentations"))
