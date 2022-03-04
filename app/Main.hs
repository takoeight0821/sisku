{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Options.Applicative
import Relude (IO, Semigroup ((<>)), join, ($))
import qualified Sisku.Commands.GenElasticIndex as GenElasticIndex
import qualified Sisku.Commands.IndexLsif as IndexLsif
import qualified Sisku.Commands.IndexLsp as IndexLsp
import qualified Sisku.Commands.Search as Search
import qualified Sisku.Commands.Server as Server

opts :: Parser (IO ())
opts =
  hsubparser $
    IndexLsif.parser
      <> IndexLsp.parser
      <> GenElasticIndex.parser
      <> Search.parser
      <> Server.parser

main :: IO ()
main = join $ execParser (info (opts <**> helper) (fullDesc <> header "Sisku - Polyglot API Search Engine"))
