module Sisku.Commands.IndexLsp (cmd, Options (..), parser) where

import Codec.Serialise (serialise)
import Control.Lens ((^.))
import qualified Data.ByteString.Lazy as BSL
import Options.Applicative
import Relude
import Sisku.App
import Sisku.Config
import Sisku.Hovercraft
import Sisku.Indexer
import Sisku.Indexer.Common
import Sisku.Indexer.ExtractCodeBlock
import Sisku.Indexer.FilterEmpty (filterEmpty)
import Sisku.Indexer.FilterHaskell (filterHaskell)
import Sisku.Indexer.ParseTreeHaskell (parseTreeHaskell)
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
    -- Run filterHaskell first, then extractCodeBlock.
    let CommonIndexer indexer = build $ filterHaskell $ extractCodeBlock $ parseTreeHaskell defaultLanguageClient
    hovercraft <- filterEmpty <$> indexer
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
  let file = dataHome </> (toString (config ^. projectId) <> ".cbor")
  liftIO $ BSL.writeFile file (serialise hc)
writeHovercraft (Just file) hc = liftIO $ BSL.writeFile file (serialise hc)
