#######################################
Future grscheller/neovim-notes factoids
#######################################

1. Use nvim as Lua REPL

   - shows 42: ``:lua vim.notify(2 * 21)``
   - shows LSP log file location: ``:lua vim.notify(vim.lsp.get_log_path())

2. See LSP problems

   - location:  ``~/.local/state/nvim/lsp.log``
   - to edit: ``:lua vim.cmd.edit(vim.lsp.get_log_path())``

3. New home LSP configs for Neoviim 0.11+

   - location ``~/.config/nvim/lsp/``
   - create ``some_lang_ls.lua``
   - have it return the same tables nvim-lspconfig would expect
