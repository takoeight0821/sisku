# Sisku

The Polyglot API Search Engine, based on LSP.

## Usage

Create a configuration file, sisku_config.json, and specify the project ID and the language server settings.

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

Generate a Sisku index file from a language server.

```sh
sisku index-lsp
```

Launch Sisku server.

```bash
sisku serve
```
Access the Sisku interface at http://localhost:8080/

![Sisku interface](https://raw.githubusercontent.com/takoeight0821/sisku/master/docs/images/sisku.png "Sisku interface")