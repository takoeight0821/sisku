# Architecture

Siskuは次の３つのコンポーネントからなる。

- sisku index-lsp
    - LSPを利用したインデクサ
- sisku serve
    - 検索エンジンのバックエンド + 検索エンジン（トークン編集距離ソート）
- sisku-elm
    - 検索エンジンのフロントエンド

<!--
![Sisku Architecture](/docs/Sisku_Architecture.jpg)
-->

## Hovercraft
Hovercraftとは、`sisiku index-lsp` が生成するデータベースファイル（JSON）だ。
フォーマットはまだ未確定だが、執筆時点のmainブランチのHEAD[daf1cc3](https://github.com/takoeight0821/sisku/commit/daf1cc3b7d1c67cb637fbd1466bac0315811e670)
では、以下のフォーマットが使用されている。

[sisku/Hovercraft\.hs at daf1cc3](https://github.com/takoeight0821/sisku/blob/daf1cc3b7d1c67cb637fbd1466bac0315811e670/src/Sisku/Hovercraft.hs)より抜粋
```haskell
data Hovercraft = Hovercraft {_projectId :: Text, _pages :: [Page]}

newtype Page = Page {_entries :: [Entry]}

data Entry = Entry
  { _document :: TextDocumentIdentifier,
    _projectId :: Text,
    _hover :: Hover,
    _definitions :: [Definition],
    _signatureToken :: [[Token]],
    _rootPath :: FilePath
  }

data Definition = Definition {_uri :: Uri, _range :: Range}

data TextDocumentIdentifier
data Hover
```

`data TextDocumentIdentifier`と`data Hover`はLSPと同じ定義です。
[LSP's TextDocumentIdentifier definition](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentIdentifier)
[LSP's Hover definition](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#hover)

シリアライズには、[CBOR](https://cbor.io/)と[JSON](https://www.json.org/)をサポートしている。

実装に利用しているライブラリは以下のとおり。
* [cborg: Concise Binary Object Representation \(CBOR\)](https://hackage.haskell.org/package/cborg)
* [aeson: Fast JSON parsing and encoding](https://hackage.haskell.org/package/aeson)