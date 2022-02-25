module Sisku.Commands.Server (parser) where

import Network.Wai.Handler.Warp (run)
import Options.Applicative
import Relude
import Sisku.Server (app)
import UnliftIO.Directory (XdgDirectory (XdgData), doesDirectoryExist, getXdgDirectory)

newtype Options = Options
  { port :: Int
  }

cmd :: Options -> IO ()
cmd Options {..} = do
  staticFilePath <- getXdgDirectory XdgData "sisku/static"
  isExists <- doesDirectoryExist staticFilePath
  unless isExists $
    error $
      "Directory " <> show staticFilePath <> " does not exist.\n"
        <> "Please install sisku-elm.\n"
  putTextLn $ "Listening on port " <> show port <> "..."
  run port (app staticFilePath)

opts :: Parser Options
opts =
  Options
    <$> option auto (long "port" <> short 'p' <> value 8080 <> showDefault <> help "Port to listen on")

parser :: Mod CommandFields (IO ())
parser = command "serve" (info (cmd <$> opts) (progDesc "Serve the site"))
