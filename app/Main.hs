{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Options.Applicative
import Relude hiding (id)
import qualified Commands.GenElasticIndex
import qualified Commands.NewIndexLsp
import qualified Commands.IndexLsp
import qualified Commands.IndexLsif
import qualified Commands.RenderMarkdown

opts :: Parser (IO ())
opts =
  hsubparser $
    Commands.IndexLsif.parser
      <> Commands.IndexLsp.parser
      <> Commands.NewIndexLsp.parser
      <> Commands.GenElasticIndex.parser
      <> Commands.RenderMarkdown.parser

main :: IO ()
main = join $ execParser (info (opts <**> helper) (fullDesc <> header "Sisku - Polyglot API Search Engine"))
