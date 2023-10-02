--[[ First set up some sane, not too opinionated options ]]

local options = {

   -- Set default fileencoding, localizations, and file formats
   fileencoding = 'utf-8',
   spelllang = 'en_us',
   fileformats = 'unix,mac,dos',

   -- Set default tab/indenting behavior
   tabstop = 4,      -- tab stops every 4 spaces, not size of tab character
   shiftwidth = 4,   -- number of columns used for auto-indentation
   softtabstop = 4,  -- <tab>/<bs> inserts/deletes 4 columns ws
   expandtab = true,  -- use only spaces when tabbing or auto-indenting

   -- Buffer/Editing preferences
   hidden = true,       -- my expectations are that buffers don't get abandoned
   joinspaces = false,  -- use 2 spaces when joining sentences
   nrformats = 'bin,hex,octal,alpha',  -- for <c-a> & <c-x>
   undofile = true,    -- save undo history in ~/.local/share/nvim/undo/
   ignorecase = true,  -- Case insensitive search when given
   smartcase = true,   -- just lower case search patterns.
   textwidth = 80,     -- keep comments & code horizontally under control
   -- colorcolumn = '+1,+21,+41',  -- keep comments <= 80 code <= 100, data <= 120
   spelloptions = 'camel',   -- spellCheckCamelCaseComponents
   formatoptions = 'tcqjo1',  -- Format comments, use insert-mode <C-u> to
                              -- quickly undo a comment header insertion.

   -- Windowing preferences
   mouse = 'a',        -- enable mouse for all modes
   ruler = false,      -- disable ruler
   splitbelow = true,  -- horizontally split window below
   splitright = true,  -- vertically split window to right
   number = false,     -- initially no window line numbering
   relativenumber = false,   -- initially no window relative line numbering
   wrap = false,       -- don't wrap lines
   scrolloff = 10,     -- try to keep cursor off of top/bottom line of window
   sidescrolloff = 8,  -- try to keep cursor away from side of window
   sidescroll = 1,     -- horizontally scroll one character at a time

   -- Settings affecting LSP clients & plugins
   termguicolors = true,  -- enable 24-bit RGB color for ISO-8613-3 terminals
   timeoutlen = 1200,  -- ms to wait for key mapped sequence to complete
   updatetime = 400,   -- ms of no cursor movement to trigger CursorHold event
   signcolumn = 'yes', -- fixes first column, reduces jitter
}

for k, v in pairs(options) do
vim.opt[k] = v
	vim.opt[k] = v
end

-- list & dictionary options
vim.opt.matchpairs:append { '<:>' }  -- additional symbols for '%' matching
vim.opt.iskeyword:append { '-' }     -- adds snake-case to word motions
vim.opt.listchars = {                -- for :list, :set list, :set nolist
   space = '_',
   trail = '*',
   nbsp = '+',
   tab = '<->',
   eol = '$',
}

--[[ Autocmds to give some useful visual feedback ]]

local autogrp = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

--[[ Text editing commands/autocmds not related to specific plugins ]]
local BuggyTextGrp = autogrp('BuggyText', { clear = true })

-- No smartcase while in cmdline mode
autocmd('CmdLineEnter', {
   pattern = '*',
   command = 'set nosmartcase noignorecase',
   group = BuggyTextGrp,
   desc = 'Use case sensitive search in command mode',
})

-- Use smartcase outside cmdline mode
autocmd('CmdLineLeave', {
   pattern = '*',
   command = 'set ignorecase smartcase',
   group = BuggyTextGrp,
   desc = 'Use smartcase when not in Command Mode',
})

autocmd('TextYankPost', {
   pattern = '*',
   callback = function()
      vim.highlight.on_yank { timeout = 500, higroup = 'Visual' }
   end,
   group = BuggyTextGrp,
   desc = 'Give visual feedback when yanking text',
})
