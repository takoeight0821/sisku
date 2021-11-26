# Architecture

Siskuは次の二つのコンポーネントからなる。

- sisku
    - LSP/LSIFを通してホバー情報を収集する。
- sisku-wui
    - 検索エンジンのフロントエンド。内部ではElasticsearchを利用している。

![Sisku Architecture](/docs/Sisku_Architecture.jpg)
