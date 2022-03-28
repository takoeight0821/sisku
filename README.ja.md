# Sisku - Polyglot API Search Engine

Siskuは多言語対応のAPI検索エンジンです。
様々なプログラミング言語で統一的にAPIドキュメントを生成、検索することができます。

![Siskuでの検索例](https://raw.githubusercontent.com/takoeight0821/sisku/master/docs/images/sisku_search_example.png)

## Install インストール方法

### Build from source

1. Siskuのソースコードをダウンロード

```bash
git clone https://github.com/takoeight0821/sisku
cd sisku
```

2. `sisku`のビルドとインストール

Sisku本体のビルドとインストールにはstack (https://docs.haskellstack.org/en/stable/README/)を使う。

```bash
stack install
```

2. `sisku-elm`のインストール

Webフロントエンドの実装である`sisku-elm`のビルドとインストールにはnpm (https://www.npmjs.com/)を使う。

```bash
cd sisku-elm

npm install

./intall.sh
```

## Usage 使い方

### Indexing インデックスの作成

プロジェクトのIDと、プロジェクトに含まれる言語および対応するLanguage Serverについての設定をsisku_config.jsonとして記述する。

```json
$ cat sisku_config.json
{
  "projectId": "com.github.takoeight0821.sisku",
  "lspSettingMap": {
    "haskell": {
      "language": "haskell",
      "root_uri_patterns": [
        "package.yaml"
      ],
      "exclude_patterns": [
        "dist-newstyle/**/*.hs"
      ],
      "command": "haskell-language-server-wrapper --lsp",
      "extensions": [
        ".hs"
      ]
    },
    "rust": {
      "language": "rust",
      "root_uri_patterns": [
        "Cargo.toml"
      ],
      "exclude_patterns": [],
      "command": "rust-analyzer",
      "extensions": [
        ".rs"
      ]
    }
  }
}
```

プロジェクトのルートディレクトリ（例えば`/path/to/takoeight0821/sisku`）で次のようなコマンドを実行すると、
インデックス情報が$XDG_DATA_HOME/sisku/hovercraft/<projectID>.jsonに保存される。

```bash
sisku index-lsp app/Main.hs
```

`app/Main.hs`の部分には、ソースファイルの探索の起点となるファイルパスを指定する。
`sisku index-lsp`は、ここで指定したファイルパスから、インデックスする言語と拡張子を決定する。

FIXME: 現在の実装では、ソースファイルの拡張子が複数ある場合（例えば`.hs`と`.lhs`が両方ある場合）、どちらか片方しかインデックスできない。

### Launch Sisku Server Siskuサーバーの起動

```bash
sisku serve
```

http://localhost:8080/ へアクセスすると、Siskuの検索画面が表示される。

![Siskuの画面](https://raw.githubusercontent.com/takoeight0821/sisku/master/docs/images/sisku.png "Siskuの画面")

## Goals 目標

1. できるかぎりあらゆるプログラミング言語に対応する：プログラミング言語ごとに、その意味を解析してAPIドキュメントのインデックスを作る処理を実装することはしない。どんなプログラミング言語にでも簡単に対応できるように、言語間の可搬性を重視する。そのために、多くのプログラミング言語がすでに対応しているLanguage Server Protocolを利用する。

2. APIの構造に基づいた検索を提供する：Siskuは、単なるコード検索ではなく、型やドキュメンテーションコメントなどの情報に注目し、APIの構造に基づいた検索を提供する。

## Core Idea 中心となるアイディア

プログラミング言語Haskellのコミュニティでは、API検索エンジンHoogleが広く愛用されている。
例えば「リストからその先頭要素を取り出す関数」を探したいとする。
Haskellプログラマであれば、この関数は`[a] -> a`という型を持つことが自然にわかる。
しかし、Googleで「Haskell [a] -> a」と検索しても有益な結果は得られない。
そこでHoogleで「[a] -> a」と検索すると、さまざまなライブラリが横断的に調べ挙げられ、
`[a] -> a`という型を持つ関数がそのドキュメントとともに列挙される。
プログラマはそれを眺めるだけで「リストからその先頭要素を取り出す関数」、例えば`Data.List.head`などを見つけることができる。

このような検索エンジンがあらゆる言語にあればいいのに、と考えるのは自然なことだろう。
しかし、Hoogleのように成熟したツールを作るのは単純な仕事ではない。
Siskuはこの仕事に切り込む。
すでに成熟した実装とコミュニティを持つLanguage Server Protocolを利用して、汎用的な"Hoogle"に挑戦する。

コードに対する全文検索エンジンはSiskuの解決したい問題には不十分だと考える。
Hoogleのようなツールを求めるプログラマが欲しいのは、実装についての情報ではなく、それを隠蔽したインターフェースの情報である。
実装の詳細は簡単に調べられるべきだが、それが検索結果として出てきてしまうのはノイズである。

Siskuは、Language Server Protocolが規定する、識別子に対するホバー情報を主な検索対象とする。
ホバー情報にはその識別子の型やドキュメンテーションコメントなどが含まれている。
多くのプログラミング言語では、これらの情報があればAPIの構造に基づいた検索を提供するのに十分であると予想している。