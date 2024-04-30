-- if telescope plugin load fails, return an empty table
-- this should still load the file even if plugin errors occur
-- READ: ap_abc.lua for plugin setup
local telescope
local success, plugin = pcall(require, 'telescope.builtin')

telescope = success and plugin or setmetatable({}, {
    __index = function()
        return function() end
    end
})

-- milky float
local function vir()
    if vim.o.virtualedit == 'all' then
        vim.o.virtualedit = ''
    else
        vim.o.virtualedit = 'all'
    end
end

-- press <C-i>, press it again to revert back to normal input
-- READ: ap_abc.lua for plugin setup
local function input_jp()
    vim.cmd(("set keymap=" .. ((vim.g.kana and "") or "kana")))
    vim.g.kana = not vim.g.kana
end

-- BEFORE THE YEAR IS OUT, WE'LL BE HARVESTING MANGOES IN TAHITI!
-- TAHITI ARFUR! TAHITI!
local function current_date()
    local date_format = os.date("%d-%m-%Y")
    vim.api.nvim_put({date_format}, "", true, true)
end

-- NOTE: the contents of this code might look weird when
-- viewing on github, column duplication or such
-- READ: hachos.lua for finalizing tables
local M = {}

M.keymaps = {
  silent = {
    -- cursor movement
    {{'v', 'n'}, 'w',   "0"},
    {{'v', 'n'}, 'ee',  "$"},
    {{'v', 'n'}, 'ew', "gM"},
    {{'v', 'n'}, 'ev',  vir},

    -- cursor travel
    {{'v', 'n'}, 'es', ":mark o <cr>"},
    {{'v', 'n'}, 'er', "'o"},

    -- dd without replacing yank data,
    {'n', 'dd', '"_dd' },

    -- filter tabs
    {'n', 'dr', ":%!expand -t2<cr>" },

    -- set japanese input
    {'n', '<C-i>', input_jp},

    -- insert current date
    {'n', '<C-d>', current_date},

    -- telescope menu
    {'n', 'ff', telescope.find_files, {} },
    {'n', 'fg', telescope.live_grep,  {} },
    {'n', 'fb', telescope.buffers,    {} },
    {'n', 'fv', telescope.help_tags,  {} },
  },
  standard = {
    -- quick search & replace
    {{'v', 'n'}, '<C-f>', ':%s/'},

    -- buffer manipulation
    {'n', '<C-z>', vim.cmd.bprev},
    {'n', '<C-a>', vim.cmd.bnext},

    -- file manipulation
    {'n', '<C-c>', ':q! <cr>'},
    {'n', '<C-s>', vim.cmd.write},
  },
}

M.options = {
    wrap          = false,
    expandtab     = false,
    hlsearch      = false,
    incsearch     = true,
    ignorecase    = true,
    smartcase     = true,
    smartindent   = true,

    number        = false,
    ruler         = false,
    showcmd       = false,
    showmode      = false,
    showtabline   = 0,
    signcolumn    = "yes:1",
    completeopt   = {"menuone", "noselect"},
    fillchars     = {eob       = " ",
                     fold      = " ",
                     horiz     = " ",
                     horizup   = " ",
                     horizdown = " ",
                     vert      = " ",
                     vertleft  = " ",
                     vertright = " ",
                     verthoriz = " "},

   -- statusline
   cmdheight     = 0,
   laststatus    = 3,
   statusline    = '%#left# %t:%{&ft} {%{b:git_branch}} [%{mode()}]%m ' ..
                   '%#center#  %=' ..
                   '%#right# Ln %l, Col %c ' ..
                   '[%{winnr()}/%{winnr("$")}]',

    -- cursor view
    mouse         = "",
    guicursor     = "i:block",
    cursorline    = true,
    scrolloff     = 8,
    sidescrolloff = 8,

    -- performance
    lazyredraw    = true,
    ttyfast       = true,
    termguicolors = true,

    -- buffer manipulation
    fileencoding  = "utf-8",
    clipboard     = "unnamedplus",

    -- file manipulation
    backup        = false,
    swapfile      = false,
    undofile      = true,
    undodir       = vim.fn.expand("~/.cache/nvim/undodir"),
}

M.theme = {
    "Normal",          "#5e5b5b",   "#fbf6f6",
    "String",          "#5e5b5b",   "NONE",

    "Constant",        "#9e9897",   "NONE",
    "Identifier",      "#8a8584",   "NONE",
    "Type",            "#5e5b5b",   "NONE",
    "PreProc",         "#5e5b5b",   "NONE",

    "Function",        "#5e5b5b",   "NONE",
    "Repeat",          "#5e5b5b",   "NONE",
    "Conditional",     "#5e5b5b",   "NONE",
    "Statement",       "#5e5b5b",   "NONE",

    "SpecialKey",      "#5e5b5b",   "NONE",
    "Special",         "#e793af",   "NONE",
    "Operator",        "#9e9897",   "NONE",
    "Float",           "#c3aa9c",   "NONE",
    "Number",          "#c3aa9c",   "NONE",
    "NonText",         "#5e5b5b",   "NONE",
    "TODO",            "#5e5b5b",   "NONE",
    "Title",           "#5e5b5b",   "NONE",
    "Underlined",      "#5e5b5b",   "NONE",

    "Comment",         "#d7d2ce",   "NONE",
    "LineNR",          "#5e5b5b",   "NONE",
    "SignColumn",      "#fbf6f6",   "NONE",
    "Search",          "#fbf6f6",   "#5e5b5b",
    "IncSearch",       "#fbf6f6",   "#5e5b5b",
    "Substitute",      "#fbf6f6",   "#5e5b5b",
    "MatchParen",      "#fbf6f6",   "#5e5b5b",
    "Visual",          "NONE",      "#f5e9da",
    "DiffAdd",         "#b3cdb9",   "#dcf5e2",
    "DiffText",        "#d7d2ce",   "NONE",
    "DiffChange",      "#d7d2ce",   "NONE",
    "DiffDelete",      "#de7f96",   "#eedada",
    "Directory",       "#5e5b5b",   "NONE",
    "Folded",          "NONE",      "#d7d2ce",

    "Error",           "#5e5b5b",   "NONE",
    "ErrorMsg",        "#5e5b5b",   "NONE",
    "WarningMsg",      "#5e5b5b",   "NONE",
    "MoreMsg",         "#5e5b5b",   "NONE",
    "Question",        "#5e5b5b",   "NONE",

    "DiagnosticError",              "#d7d2ce",   "NONE",
    "DiagnosticWarn",               "#d7d2ce",   "NONE",
    "DiagnosticHint",               "#d7d2ce",   "NONE",
    "CmpMaster",                    "#cac2c2",   "#f5f0f0", -- f6f1f1
    "CmpDefault",                   "#cac2c2",   "NONE",
    "CmpSpecial",                   "#cac2c2",   "NONE",
    "CmpItemKind",                  "#d0d9d6",   "NONE",
    "CmpItemAbbrDeprecatedDefault", "#d0d9d6",   "NONE",

    "@ibl.indent.char.1",           "#f3eeee",   "NONE",
    "@ibl.whitespace.char.1",       "#f3eeee",   "NONE",

    "Tabline",         "#5e5b5b",   "#fbf6f6",
    "CursorLine",      "NONE",      "#f5f0f0",
    "CursorLineNC",    "#5e5b5b",   "NONE",

-- Statusline: all NONE order fixes the caret appearing issue.
    "Statusline",      "NONE",      "NONE",
    "StatuslineNC",    "#5e5b5b",   "NONE",
    "left",            "#9d9ea0",   "#e1e7f5",
    "center",          "#e1e7f5",   "#e1e7f5",
    "right",           "#9d9ea0",   "#e1e7f5",
}

M.source_full = {
    -- EDITOR ENLIGHTENMENT
    'nvim-treesitter/nvim-treesitter',
    'nvim-telescope/telescope.nvim',
    'nvim-lua/plenary.nvim',
    'echasnovski/mini.starter',

    -- EDITOR ENCHANCEMENT
    'mg979/vim-visual-multi',

    -- EDITOR ASSISTANT
    'williamboman/mason.nvim',
    'williamboman/mason-lspconfig',
    'neovim/nvim-lspconfig',
    'hrsh7th/nvim-cmp',
    'hrsh7th/cmp-path',
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-nvim-lua',
    'hrsh7th/cmp-nvim-lsp',
    'L3MON4D3/LuaSnip',
    'saadparwaiz1/cmp_luasnip',

    -- SYNTAX HIGHLIGHTING
    'lewis6991/gitsigns.nvim',
    'brenoprata10/nvim-highlight-colors',
    'lukas-reineke/indent-blankline.nvim'
}

M.source_raw = {
    -- ALTERNATIVE INPUT LANGUAGE
    'neovim/neovim/master/runtime/keymap/kana.vim',
    'skk-dev/dict/master/SKK-JISYO.L'
}

return M
