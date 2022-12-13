module Sisku.Tree.Parser where

import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.String qualified as String
import Relude
import Sisku.Token as Sisku
import Text.Megaparsec

data TokenStream = TokenStream
  { tokenStreamInput :: String,
    unTokenStream :: [WithPos Sisku.Token]
  }

instance Stream TokenStream where
  type Token TokenStream = WithPos Sisku.Token
  type Tokens TokenStream = [WithPos Sisku.Token]
  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy xs = xs
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (TokenStream _ []) = Nothing
  take1_ (TokenStream str (t : ts)) = Just (t, TokenStream (drop (tokensLength pxy (t :| [])) str) ts)
  takeN_ n (TokenStream str s)
    | n <= 0 = Just ([], TokenStream str s)
    | null s = Nothing
    | otherwise =
        let (x, s') = splitAt n s
         in case NE.nonEmpty x of
              Nothing -> Just (x, TokenStream str s')
              Just nex -> Just (x, TokenStream (drop (tokensLength pxy nex) str) s')
  takeWhile_ f (TokenStream str s) =
    let (x, s') = span f s
     in case NE.nonEmpty x of
          Nothing -> (x, TokenStream str s')
          Just nex -> (x, TokenStream (drop (tokensLength pxy nex) str) s')

instance VisualStream TokenStream where
  showTokens Proxy =
    String.unwords
      . NE.toList
      . fmap show
  tokensLength Proxy xs = sum (fmap _length xs)

instance TraversableStream TokenStream where
  reachOffset o PosState {..} =
    ( Just (prefix <> restOfLine),
      PosState
        { pstateInput = TokenStream {tokenStreamInput = postStr, unTokenStream = post},
          pstateOffset = max pstateOffset o,
          pstateSourcePos = newSourcePos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = prefix
        }
    )
    where
      prefix =
        if sameLine
          then pstateLinePrefix <> preLine
          else preLine
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> pstateSourcePos
          (x : _) -> _startPos x
      (pre, post) = splitAt (o - pstateOffset) (unTokenStream pstateInput)
      (preStr, postStr) = splitAt tokensConsumed (tokenStreamInput pstateInput)
      preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
      tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre
      restOfLine = takeWhile (/= '\n') postStr

pxy :: Proxy TokenStream
pxy = Proxy

type Parser = Parsec Void TokenStream

-- | Parse an `Ident`
pIdent :: Parser Sisku.Token
pIdent = token test Set.empty <?> "ident"
  where
    test (WithPos _ _ _ (Ident x)) = Just (Ident x)
    test _ = Nothing

pSymbol :: Text -> Parser Sisku.Token
pSymbol sym = token test Set.empty <?> "symbol"
  where
    test (WithPos _ _ _ (Symbol x)) | x == sym = Just (Symbol x)
    test _ = Nothing