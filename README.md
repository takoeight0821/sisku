# Sisku

The API Search Engine using LSP.

## Usage

Generate a Sisku index file from LSIF index file.

```sh
sisku index-lsif dump.lsif -o hovercraft.json
```

Generate a Sisku index file from a language server.

```sh
sisku index-lsp -s lsp-settings.json -o hovercraft.json
```

Convert a Sisku index file to JSONL for Elasticsearch Bulk API.

```sh
sisku gen-elastic-index -i hovercraft.json -o index.jsonl
```

Launch Elasticsearch server and load index.jsonl.

```
elasticsearch

curl -H "Content-Type: application/json" -XPOST "localhost:9200/hovercraft/_bulk" --data-binary @index.jsonl
```

Start Sisku server.

```
cd sisku-wui

npm start
```
