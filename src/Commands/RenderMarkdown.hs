{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Commands.RenderMarkdown (parser) where

import qualified Data.Aeson as Aeson
import Relude
import Options.Applicative
import qualified Data.ByteString.Lazy as B
import Hovercraft
import qualified Data.Text.IO as T

data Options = Options
    {
        inputFile :: FilePath,
        outputFile :: FilePath
    }
    
cmd :: Options -> IO ()
cmd Options{..} = do
    hs <- Aeson.decode <$> B.readFile inputFile 
    case hs of
        Nothing -> error "Could not decode JSON"
        Just hs -> do
            let md = renderAsMarkdown hs
            T.writeFile outputFile md

opts :: Parser Options
opts =
    Options
        <$> strArgument (metavar "INPUT_FILE")
        <*> strOption (long "output" <> short 'o' <> metavar "OUTPUT_FILE" <> value "out.md")

parser :: Mod CommandFields (IO ())
parser = command "render-markdown" (info (cmd <$> opts) (progDesc "Render hovercraft as Markdown"))