# Son of Anton

Son of Anton is PiperScrips LSP implementation.

See the [specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)

Server is a JSON-RPC server over stdin and stdout.

To install:
`cargo install --package son_of_anton`

To use in i.e. neovim:

```lua
local root_dir = vim.fs.dirname(vim.fs.find({ ".git" }, { upward = true })[1])
local client = vim.lsp.start({
	name = "anton",
	cmd = { "son_of_anton" },
	root_dir = root_dir,
})
vim.lsp.buf_attach_client(0, client)
```

## Features

Currently supports these capabilites:

- Intitialize
