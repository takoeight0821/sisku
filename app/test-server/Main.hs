{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Lens ((^.))
import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Lens (uri)
import Relude

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SInitialized $ const $ pure (),
      requestHandler STextDocumentHover $ \req responder -> do
        let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
            Position _l _c' = pos
            rsp = Hover ms (Just range)
            ms = HoverContents $ markedUpContent "test" "Hello world"
            range = Range pos pos
        responder (Right $ Just rsp),
      requestHandler STextDocumentDefinition $ \req responder -> do
        let RequestMessage _ _ _ (DefinitionParams doc _ _ _) = req
            rsp = Location (doc ^. uri) (Range (Position 0 0) (Position 0 0))
        responder (Right $ InL rsp),
      requestHandler STextDocumentDocumentSymbol \_req responder -> do
        let rsp =
              List
                [ DocumentSymbol
                    { _name = "test_symbol",
                      _detail = Just "Test symbol detail",
                      _kind = SkVariable,
                      _tags = Nothing,
                      _deprecated = Nothing,
                      _range = Range (Position 0 0) (Position 0 0),
                      _selectionRange = Range (Position 0 0) (Position 0 0),
                      _children = Nothing
                    }
                ]
        responder (Right $ InL rsp)
    ]

main :: IO Int
main =
  runServer $
    ServerDefinition
      { onConfigurationChange = const $ const $ Right (),
        defaultConfig = (),
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = handlers,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions
      }
