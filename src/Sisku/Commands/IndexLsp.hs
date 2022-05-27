module Sisku.Commands.IndexLsp (cmd, Options (..), parser) where

import Control.Lens ((^.))
import qualified Data.Aeson as Aeson
import Options.Applicative
import Relude
import Sisku.App
import Sisku.Config
import Sisku.Hovercraft
import Sisku.Indexer
import Sisku.Indexer.Exhaustive
import Sisku.Indexer.ExtractCodeBlock
import Sisku.Indexer.FilterHaskell (filterHaskell)
import System.FilePath ((</>))
import UnliftIO.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)

data Options = Options
  { configFilePath :: FilePath,
    outputFilePath :: Maybe FilePath,
    debugMode :: Bool
  }

cmd :: Options -> IO ()
cmd Options {..} = do
  config <- loadConfig configFilePath
  runSiskuApp config do
    -- 最初にfilterHaskellが適用され、そのあとextractCodeBlockが適用される
    -- ここでは関数合成をしている！適用順は見た目と逆！
    let ExhaustiveIndexer indexer = build (defaultLanguageClient & extractCodeBlock & filterHaskell)
    hovercraft <- indexer
    when debugMode $
      print hovercraft
    writeHovercraft outputFilePath hovercraft

opts :: Parser Options
opts =
  Options
    <$> strOption (short 'c' <> long "config" <> metavar "<config>" <> help "Path to the config file" <> value "sisku_config.json")
    <*> optional (strOption (short 'o' <> long "output" <> metavar "<output>" <> help "Path to the output file"))
    <*> switch (short 'd' <> long "debug" <> help "Enable debug mode")

parser :: Mod CommandFields (IO ())
parser = command "index-lsp" (info (cmd <$> opts) (progDesc "Make a index via LSP."))

-- | Get XDG_DATA_HOME
getDataHome :: IO FilePath
getDataHome = getXdgDirectory XdgData "sisku/hovercraft"

-- | Write hovercraft to file
writeHovercraft :: (MonadSiskuApp m, MonadIO m) => Maybe FilePath -> Hovercraft -> m ()
writeHovercraft Nothing hc = do
  dataHome <- liftIO getDataHome
  liftIO $ createDirectoryIfMissing True dataHome
  config <- getConfig
  let file = dataHome </> (toString (config ^. projectId) <> ".json")
  liftIO $ Aeson.encodeFile file hc
writeHovercraft (Just file) hc = liftIO $ Aeson.encodeFile file hc
