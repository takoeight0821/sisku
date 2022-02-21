{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import qualified Commands.GenElasticIndex
import qualified Commands.IndexLsif
import qualified Commands.IndexLsp
import qualified Commands.Search
import qualified Commands.Server
import Options.Applicative
import Relude (IO, Semigroup ((<>)), join, ($))

opts :: Parser (IO ())
opts =
  hsubparser $
    Commands.IndexLsif.parser
      <> Commands.IndexLsp.parser
      <> Commands.GenElasticIndex.parser
      <> Commands.Search.parser
      <> Commands.Server.parser

main :: IO ()
main = join $ execParser (info (opts <**> helper) (fullDesc <> header "Sisku - Polyglot API Search Engine"))
