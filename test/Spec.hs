{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value (Null))
import qualified Data.Aeson as Aeson
import Hovercraft
import Language.LSP.Types
import Relude
import System.Directory.Extra (makeAbsolute)
import System.FilePath
import System.Process (callProcess)
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
  -- Generate a hovercraft file for testcases/hello.test.
  testcase <- makeAbsolute "test/testcases/hello.test"
  callProcess "sisku" ["index-lsp", testcase, "--output", testcase ++ ".json", "--config", "test/testcases/sisku_config.json"]
  let rootPath = takeDirectory testcase
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
                      _moniker = Null,
                      _rootPath = rootPath
                    }
                ]
            }
        ]
    }
