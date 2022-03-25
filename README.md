# Sisku

The Polyglot API Search Engine, based on LSP.

## Features

Token-based search for signatures.

![Token-based search for signatures](https://raw.githubusercontent.com/takoeight0821/sisku/main/docs/images/token-based.png)

Fuzzy search for documentations.

![Fuzzy search for documentations](https://raw.githubusercontent.com/takoeight0821/sisku/main/docs/images/fuzzy.png)

## Installation

### Prerequisites

* [stack](https://haskellstack.org)
* [npm](https://www.npmjs.com/)

### Download the source code

```bash
git clone https://github.com/takoeight0821/sisku
cd sisku
```

### Build `sisku` and Install

```bash
stack install
```

### Build `sisku-elm` and Install

```bash
cd sisku-elm
npm install
./install.sh
```

`install.sh` runs `npx parcel build src/index.html` and copies the artifact to `$XDG_DATA_HOME/sisku/static`.
If `XDG_DATA_HOME` is not set, the artifact will be copied to `$HOME/.local/share/sisku/static`.

## Usage

First, create a configuration file, sisku_config.json, and specify the project ID and the language server settings.

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

Then, generate a Sisku index file using the language server.
Index files will be stored as `$XDG_DATA_HOME/sisku/hovercraft/<projectId>.json`.

```sh
sisku index-lsp
```

Launch Sisku server.

```bash
sisku serve -p 8080
```

Access the Sisku interface at http://localhost:8080/

![Sisku interface](https://raw.githubusercontent.com/takoeight0821/sisku/master/docs/images/sisku.png "Sisku interface")

## Configuaration

The default name of the configuration file is `sisku_config.json`.
You can also specify the configuration file by `--config` option.

```bash
sisku index-lsp --config sisku_config.json
```

This is the list of config fields:

* projectId (type: string)

		The identifier of the project. It must be unique across all projects indexed in Sisku.

* lspSettingMap (type: object)
		
		Keys are only used internally, so it can be anything that is not duplicated.

* lspSettingMap.<key>.language (type: string)
		
		Language name

* lspSettingMap.<key>.root_uri_patterns (type: string[])
		
		Patterns with file or directory names for finding root_uri.

		(See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initializeParams )

* lspSettingMap.<key>.exclude_patterns (type: string[])

		Patterns specifying files to be excluded from indexing.

* lspSettingMap.<key>.command (type: string)

		Command to launch a Language Server

* lspSettingMap.<key>.extensions (type: string[])

		List of source code extensions.