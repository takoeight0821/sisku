{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Sisku.TreeSitter.ParseHaskell where

import Foreign.C.String
import Foreign.Marshal.Alloc (malloc)
import Foreign.Marshal.Array (mallocArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke)
import Relude
import TreeSitter.Haskell
import TreeSitter.Node
import TreeSitter.Parser
import TreeSitter.Tree

parseHaskell :: IO ()
parseHaskell = do
  parser <- ts_parser_new
  _ <- ts_parser_set_language parser tree_sitter_haskell

  let source = "module Test (main) where\nimport Lib\nf1 = undefined\nf2 = undefined"

  (str, len) <- newCStringLen source
  tree <- ts_parser_parse_string parser nullPtr str len

  n <- malloc
  ts_tree_root_node_p tree n

  putStrLn "module (root) ---------"
  Node {..} <- peek n
  let childCount = fromIntegral nodeChildCount

  children <- mallocArray childCount
  tsNode <- malloc
  poke tsNode nodeTSNode
  ts_node_copy_child_nodes tsNode children

  printChildren children childCount

  putStrLn "where ---------"
  Node {..} <- peekElemOff children 3
  let nextChildCount = fromIntegral nodeChildCount

  nextChildren <- mallocArray nextChildCount
  nextTsNode <- malloc
  poke nextTsNode nodeTSNode
  ts_node_copy_child_nodes nextTsNode nextChildren

  printChildren nextChildren nextChildCount

  putStrLn "END"

printChildren :: Ptr Node -> Int -> IO ()
printChildren children count = for_
  [0 .. count - 1]
  \n -> do
    child <- peekElemOff children n
    printNode child

printNode :: Node -> IO ()
printNode n = do
  theType <- peekCString $ nodeType n
  let TSPoint {..} = nodeStartPoint n
      start = "(" <> show pointRow <> "," <> show pointColumn <> ")"
  let TSPoint {..} = nodeEndPoint n
      end = "(" <> show pointRow <> "," <> show pointColumn <> ")"
  putStrLn $ theType <> start <> "-" <> end
