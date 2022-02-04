module Server (app) where

import Relude
import Servant (Application, Raw, Server, serve, serveDirectoryWebApp)

type API = Raw

api :: Proxy API
api = Proxy

server :: FilePath -> Server API
server = serveDirectoryWebApp

app :: FilePath -> Application
app staticFilePath = serve api (server staticFilePath)