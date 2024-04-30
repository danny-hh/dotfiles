-- nvim-treesitter/nvim-treesitter
require 'nvim-treesitter.configs'.setup {
    ensure_installed = {
      'lua'
    },

    ignore_install = {
      'cpp',
      'javascript',
      'typescript',
      'npm',
      'python',
      'vim',
      'vimdoc'
    },

    sync_install = true,
    auto_install = false,

  highlight = {
    enable = true
  },
}

-- nvim-telescope/telescope.nvim
-- + nvim-lua/plenary.nvim
require('telescope').setup{
  pickers = {
    find_files = {
      hidden = true,
      theme = "dropdown",
    }
  },
}

vim.o.timeout    = true;
vim.o.timeoutlen = 300;


-- brenoprata10/nvim-highlight-colors
require("nvim-highlight-colors").setup {
    render = 'background',
    enable_named_colors = false,
    enable_tailwind = false,
}

-- lewis6991/gitsigns.nvim
require('gitsigns').setup {
    signs = {
      add          = { text = '++' },
      change       = { text = '::' },
      delete       = { text = '--' },
      changedelete = { text = '~~' },
      topdelete    = { text = 'xx' },
      untracked    = { text = '..' },
    },
      signcolumn = true,
      numhl      = false,
      linehl     = false,
      word_diff  = false,

    watch_gitdir = {
      follow_files = false
    },
}

-- lukas-reineke/indent-blankline.nvim.git
require("ibl").setup {
  scope = { enabled = false },
}

require "ibl".overwrite {
  exclude = { filetypes = {"text", "markdown"} }
}

-- kana.vim, written by Rory McCann
local basepath = os.getenv('HOME') .. "/.config/nvim/pack/plugins/start/skk/"

vim.g.eskk_directory = {
    path = basepath,
}

vim.g.eskk_dictionary = {
    path = basepath .. "SKK-JISYO.L",
    sorted = 1,
    encoding = "utf-8",
}

vim.g.eskk_large_dictionary = {
    path = basepath .. "SKK-JISYO.L",
    sorted = 1,
    encoding = "euc-jp",
}

vim.g.eskk_kakutei_when_unique_candidate = 1
vim.g.eskk_enable_completion = 0
vim.g.eskk_no_default_mappings = 1
vim.g.eskk_keep_state = 0
vim.g.eskk_egg_like_newline = 1

-- echasnovski/mini.starter
local H = {}

H.default_header = function()
  local hour = tonumber(vim.fn.strftime('%H'))
  local part_id = math.floor((hour + 4) / 8) + 1
  local day_part = ({ 'evening', 'morning', 'afternoon', 'evening' })[part_id]
  local username = vim.loop.os_get_passwd()['username'] or 'USERNAME'

  return ('good %s, %s!'):format(day_part, username)
end

H.default_footer = [[
   ,-_,.
 ,( _  ))
 7 (_) 77
 ((   :))
  ~__>~'
   cY?'
   `l,__
    l7-'
   ;l
   _i_,
  l___l
  \___/
        irhl
]]

local starter = require('mini.starter')
starter.setup({
  header = H.default_header,
  footer = H.default_footer,
  evaluate_single = true,

items = {
    starter.sections.recent_files(5, false, false),

     { section = "Bookmarks",
       name    = "Aseprite",
       action  = "!bb $HOME/.local/share/Steam/steamapps/common/Aseprite/aseprite"},

     { section = "Bookmarks",
       name    = "Steam Client",
       action  = "!bb steam"},
  },
  content_hooks = {
    starter.gen_hook.adding_bullet(),
    starter.gen_hook.aligning('center', 'center'),
    -- starter.gen_hook.indexing('all', { 'Builtin actions' }),
    -- starter.gen_hook.padding(3, 2),
  },
})
