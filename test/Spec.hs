{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value (Array, Object, String))
import qualified Data.Aeson as Aeson
import Language.LSP.Types
import Relude
import Sisku.Commands.IndexLsp (Options (Options, configFilePath, outputFilePath))
import qualified Sisku.Commands.IndexLsp as IndexLsp
import Sisku.Hovercraft
import System.Directory.Extra (getCurrentDirectory, makeAbsolute, setCurrentDirectory)
import System.FilePath
import Test.Hspec

main :: IO ()
main = do
  (rootPath, uri) <- initialize
  hspec $
    describe "index-lsp" $ do
      hovercraft <- runIO $ Aeson.decodeFileStrict "test/testcases/hello.test.json"
      it "hello.test" $ hovercraft `shouldBe` Just (helloTestHovercraft rootPath uri)

-- | Initialize the test suite.
initialize :: IO (FilePath, Uri)
initialize = do
  currentDirectory <- getCurrentDirectory
  setCurrentDirectory "test/testcases"
  -- Generate a hovercraft file for testcases/hello.test.
  testcase <- makeAbsolute "hello.test"
  IndexLsp.cmd
    Options
      { configFilePath = "sisku_config.json",
        outputFilePath = Just $ testcase <> ".json"
      }
  let rootPath = takeDirectory testcase

  setCurrentDirectory currentDirectory
  pure (rootPath, Uri {getUri = toText $ "file://" <> testcase})

helloTestHovercraft :: FilePath -> Uri -> Hovercraft
helloTestHovercraft rootPath uri =
  Hovercraft
    { _projectId = "com.github.takoeight0821.sisku.test",
      _pages =
        [ Page
            { _entries =
                [ Entry
                    { _document = TextDocumentIdentifier {_uri = uri},
                      _projectId = "com.github.takoeight0821.sisku.test",
                      _hover =
                        Hover
                          { _contents = HoverContents $ MarkupContent {_kind = MkMarkdown, _value = "\n```test\nHello world\n```\n"},
                            _range = Just $ mkRange 0 1 0 1
                          },
                      _definitions = [Definition {_uri = uri, _range = mkRange 0 0 0 0}],
                      _otherValues = [Object (fromList [("signature", Array [Object (fromList [("_identifier", String "Hello"), ("tag", String "Ident")]), Object (fromList [("_identifier", String "world"), ("tag", String "Ident")])])])],
                      _rootPath = rootPath
                    }
                ]
            }
        ]
    }
