module Sisku.Indexer.ParseTreeHaskell (parseTreeHaskell) where

import Data.Traversable (for)
import Relude
import Sisku.Hovercraft
import Sisku.Indexer
import Sisku.Token (Token (..), WithPos (..))
import Sisku.Tree.Parser
import Sisku.Tree.Type
import System.IO (hPutStrLn)
import Text.Megaparsec (anySingle, errorBundlePretty, parse)

parseTreeHaskell :: LanguageClient -> LanguageClient
parseTreeHaskell = onDecorate $ \super Entry {..} -> do
  if _language == "haskell"
    then do
      _typeTree <-
        catMaybes <$> for _signatureToken \(str, tokens) -> do
          let stream = TokenStream (toString str) tokens
          case parse pSignature "" stream of
            Left err -> do
              liftIO $ hPutStrLn stderr $ errorBundlePretty err
              pure Nothing
            Right tree -> pure $ Just tree
      super Entry {..}
    else super Entry {..}

-- | Parse 'ident :: token*'
pSignature :: Parser Tree
pSignature = do
  ident <- pIdent
  _ <- pSymbol ":"
  _ <- pSymbol ":"
  tokens <- some anySingle
  pure $ Branch [Leaf (Symbol "::"), Leaf ident, Branch $ map (Leaf . _value) tokens]
