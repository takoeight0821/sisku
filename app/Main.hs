module Main where

import Relude
import qualified Data.Aeson as Aeson
import Lib

main :: IO ()
main = do
  [filePath] <- getArgs
  index <- loadFile filePath
  putLBS $ Aeson.encode $ Aeson.Array $ fromList $ map (searchResultToJson index) (search index)
