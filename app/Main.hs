{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Options.Applicative
import Relude (IO, Semigroup ((<>)), join, ($))
import qualified Sisku.Commands.IndexLsp as IndexLsp
import qualified Sisku.Commands.Server as Server

opts :: Parser (IO ())
opts =
  hsubparser $
    IndexLsp.parser
      <> Server.parser

main :: IO ()
main = join $ execParser (info (opts <**> helper) (fullDesc <> header "Sisku - Polyglot API Search Engine"))
