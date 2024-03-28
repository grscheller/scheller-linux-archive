# Neovim plugin Factoids

## Third party packages

These plugins manage 3rd party tools used by other plugins.

Some plugins, like nvim-metals or rust-tools.nvim, will either invoke
lspconfig, dap, or null-ls directly themselves, or configure the Neovim
LSP client directly.

### Nvim LspConfig

The nvim-lspconfig plugin is is used to configure the built-in Neovim
LSP client. It provides "cooked in" configurations for a variety of lsp
servers. These server specific configurations can be adjusted by
providing their setup function an opts table, but otherwise are fairly
generic.

### Nvim Dap

The nvim-dap plugin is a DAP client. It has no notion of "default" or
"builtin" configurations and must be configured for the dap server
invoked, usually by some build system.

### NullLs

The null-ls plugin acts like a language server by running external
programs like linters, formatters, syntax checkers and providing their
output to the built in Neovim LSP client.

Null-ls has a number of "built in" configurations for this. Users can
also define their own configurations. I suspect this plugin configures
nvim's builtin LSP client directly itself.

### Mason

The Mason plugin is a 3rd party package manager for language servers,
null-ls built-ins (like linters or formatters), and dap servers. It
only installs these servers, it does not configure them.

The PATH inherited from the shell invoking external tools will begin
~/.local/share/nvim/mason/bin ensuring nvim first find mason installed
tools for lspconfig, dap, and NullLS first.

Package managers and configuration tools should not be too tightly
coupled together with each other.

### LazyLsp

Lazy-lsp is a Nix based 3rd party package manager.
