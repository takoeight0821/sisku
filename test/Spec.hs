{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

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
      hovercraft <- runIO $ Aeson.decodeFileStrict "test/testcases/test-server/hello.test.json"
      it "hello.test" $ hovercraft `shouldBe` Just (helloTestHovercraft rootPath uri)

-- | Initialize the test suite.
initialize :: IO (FilePath, Uri)
initialize = do
  currentDirectory <- getCurrentDirectory
  setCurrentDirectory "test/testcases/test-server"
  -- Generate a hovercraft file for testcases/test-server/hello.test.
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
                          { _contents =
                              HoverContents
                                MarkupContent
                                  { _kind = MkMarkdown,
                                    _value = "\n```test\nHello world\n```\n"
                                  },
                            _range =
                              Just
                                Range
                                  { _start =
                                      Position
                                        { _line = 0,
                                          _character = 0
                                        },
                                    _end =
                                      Position
                                        { _line = 0,
                                          _character = 0
                                        }
                                  }
                          },
                      _definitions =
                        [ Definition
                            { _uri = uri,
                              _range =
                                Range
                                  { _start =
                                      Position
                                        { _line = 0,
                                          _character = 0
                                        },
                                    _end =
                                      Position
                                        { _line = 0,
                                          _character = 0
                                        }
                                  }
                            }
                        ],
                      _signatureToken = [[Ident {_identifier = "Hello"}, Ident {_identifier = "world"}]],
                      _otherValues = [],
                      _rootPath = rootPath
                    }
                ]
            }
        ]
    }
