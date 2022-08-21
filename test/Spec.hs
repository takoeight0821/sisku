{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

import Codec.Serialise (DeserialiseFailure, deserialiseOrFail)
import Control.Lens (view, (^.))
import qualified Data.ByteString.Lazy as BSL
import Data.Either.Extra (fromRight')
import Language.LSP.Types
import Relude
import Sisku.Commands.IndexLsp (Options (Options, configFilePath, outputFilePath))
import qualified Sisku.Commands.IndexLsp as IndexLsp
import Sisku.Hovercraft
import Sisku.Search
import Sisku.Token
import System.Directory.Extra (getCurrentDirectory, makeAbsolute, setCurrentDirectory)
import System.FilePath
import Test.Hspec
import Text.Megaparsec (SourcePos (..), mkPos)

main :: IO ()
main = do
  (rootPathTestServer, uriTestServer) <- initializeTestServer
  _ <- initializeTestHLS
  hspec $
    describe "index-lsp" $ do
      hovercraft <- runIO $ deserialiseOrFail <$> BSL.readFile "test/testcases/test-server/hello.test.cbor"
      it "hello.test" $ hovercraft `shouldBe` Right (helloTestHovercraft rootPathTestServer uriTestServer)
      hovercraft <- runIO $ deserialiseOrFail <$> BSL.readFile "test/testcases/test-haskell-language-server/test-haskell-language-server.cbor"
      it "haskell-language-server" $ isRight (hovercraft :: Either DeserialiseFailure Hovercraft) `shouldBe` True
      hovercraft <- pure $ fromRight' hovercraft
      entries <- pure $ concatMap (view entries) $ hovercraft ^. pages
      it "search" $ (search "_" entries "IO" /= []) `shouldBe` True

-- | Initialize test-haskell-language-server
initializeTestHLS :: IO FilePath
initializeTestHLS = do
  currentDirectory <- getCurrentDirectory
  let testPath = currentDirectory </> "test/testcases/test-haskell-language-server"
  setCurrentDirectory testPath
  -- Generate a hovercraft file for test-haskell-language-server
  let hovercraftFilePath = testPath </> "test-haskell-language-server.cbor"
  IndexLsp.cmd
    Options
      { configFilePath = "sisku_config.json",
        outputFilePath = Just hovercraftFilePath,
        debugMode = False
      }
  let rootPath = takeDirectory hovercraftFilePath
  setCurrentDirectory currentDirectory
  pure rootPath

-- | Initialize test-server.
initializeTestServer :: IO (FilePath, Uri)
initializeTestServer = do
  currentDirectory <- getCurrentDirectory
  setCurrentDirectory "test/testcases/test-server"
  -- Generate a hovercraft file for testcases/test-server/hello.test.
  testcase <- makeAbsolute "hello.test"
  IndexLsp.cmd
    Options
      { configFilePath = "sisku_config.json",
        outputFilePath = Just $ testcase <> ".cbor",
        debugMode = False
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
                      _language = "test",
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
                                          _character = 1
                                        },
                                    _end =
                                      Position
                                        { _line = 0,
                                          _character = 1
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
                      _signatureToken =
                        [ ( "Hello world\n",
                            [ WithPos (SourcePos "" (mkPos 1) (mkPos 1)) (SourcePos "" (mkPos 1) (mkPos 7)) 6 Ident {_identifier = "Hello"},
                              WithPos (SourcePos "" (mkPos 1) (mkPos 7)) (SourcePos "" (mkPos 2) (mkPos 1)) 6 Ident {_identifier = "world"}
                            ]
                          )
                        ],
                      _typeTree = []
                    }
                ]
            }
        ]
    }
