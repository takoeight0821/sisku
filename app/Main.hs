{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import qualified Data.Aeson as Aeson
import Lib
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Options.Applicative
import Relude hiding (id)
import Servant.Server (serve)

data SiskuOption = SiskuOption
  { filePath :: FilePath,
    query :: String,
    isServerMode :: Bool,
    isLspClientMode :: Bool
  }

siskuOption :: Parser SiskuOption
siskuOption =
  SiskuOption
    <$> strOption (metavar "INDEX_FILE" <> long "index" <> value "")
    <*> strOption (metavar "QUERY" <> long "query" <> value "")
    <*> switch (long "server")
    <*> switch (long "lsp-client")

main :: IO ()
main = do
  opt <- execParser opts
  if
      | isServerMode opt -> do
        index <- loadFile (filePath opt)
        run 8080 (simpleCors $ serve (Proxy :: Proxy SearchApi) (searchServer index))
      | isLspClientMode opt -> runLspClient "deno" ["lsp"]
      | otherwise -> do
        index <- loadFile (filePath opt)
        putLBS $ Aeson.encode $ Aeson.Array $ fromList $ map Aeson.toJSON (search index (filterByQuery $ query opt))
  where
    opts =
      info
        (siskuOption <**> helper)
        (fullDesc <> progDesc "Sisku" <> header "sisku")

-- data ClientEnv = ClientEnv {hin :: Handle, hout :: Handle, idSource :: IORef Int}
-- 
-- runLspClient :: String -> [String] -> IO ()
-- runLspClient cmd args = do
--   (Just hin, Just hout, _, _) <- createProcess (proc cmd args) {std_in = CreatePipe, std_out = CreatePipe}
--   hSetBuffering hin NoBuffering
--   hSetBuffering hout NoBuffering
--   idSource <- newIORef 0
--   usingReaderT ClientEnv {hin, hout, idSource} do
--     _ <-
--       sendRequest
--         "initialize"
--         ( object
--             [ "capabilities" .= object [],
--               "initializationOptions" .= object ["enable" .= True]
--             ]
--         )
--     res <- receive
--     print res
--     _ <- sendRequest "initialized" ()
--     _ <-
--       sendRequest "textDocument/didOpen" $
--         object
--           [ "textDocument"
--               .= object
--                 [ "uri" .= String "file:///tmp/a.ts",
--                   "languageId" .= String "typescript",
--                   "version" .= Number 1,
--                   "text" .= String "Deno.\n"
--                 ]
--           ]
--     res <- receive
--     print res
--     sendResponse (res ^?! key "id") True -- (res ^?! key "params" . key "items")
--     pure ()
-- 
-- newId :: (MonadReader ClientEnv m, MonadIO m) => m Int
-- newId = do
--   idSource <- asks idSource
--   id <- readIORef idSource
--   modifyIORef idSource (+ 1)
--   pure id
-- 
-- sendRequest :: (MonadIO m, MonadReader ClientEnv m, ToJSON v) => Text -> v -> m Int
-- sendRequest method msg = do
--   id <- newId
--   let dat = object ["jsonrpc" .= String "2.0", "id" .= id, "method" .= String method, "params" .= msg]
--   send dat
--   pure id
-- 
-- sendResponse :: (MonadIO m, MonadReader ClientEnv m, ToJSON v1, ToJSON v2) => v1 -> v2 -> m ()
-- sendResponse id result = send $ object ["jsonrpc" .= String "2.0", "id" .= id, "result" .= result]
-- 
-- send :: (MonadIO m, MonadReader ClientEnv m, ToJSON a) => a -> m ()
-- send dat = do
--   let datStr :: Text = decodeUtf8 $ encode dat
--   let packet = "Content-Length: " <> show (Text.length datStr) <> "\r\nContent-Type: application/vscode-jsonrpc-harset=utf-8\r\n\r\n" <> datStr
--   hin <- asks hin
--   liftIO $ Text.hPutStr hin packet
--   liftIO $ Text.putStrLn packet
--   hFlush hin
-- 
-- receive :: (MonadIO m, MonadReader ClientEnv m) => m Value
-- receive = do
--   hout <- asks hout
--   header <- liftIO $ hGetLine hout
--   let len =
--         if "Content-Length: " `isPrefixOf` header
--           then read (drop (length ("Content-Length: " :: String)) header)
--           else -1
--   _ <- liftIO $ hGetLine hout
--   str <- liftIO $ replicateM len (hGetChar hout)
--   case decode (encodeUtf8 str) of
--     Just x -> pure x
--     Nothing -> error "invalid response"
