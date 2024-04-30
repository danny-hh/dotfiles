--[[;@

[?] DOWNLOADING PLUGINS
:lua git()

+-----------------------+
|                       |
|   STARTUP             |
|   -------             |
|   init.lua            |
|                       |
|   USER CONFIG         |
|   ----------          |
|   config.lua          |
|   hachos.lua          |
|                       |
|   GENERAL AUTOCMDS    |
|   ----------------    |
|   au.lua              |
|                       |
|   USER AFTERPLUGINS   |
|   -----------------   |
|   ap.lua              |
|   ap_lsp.lua          |
|   ap_cmp.lua          |
|                       |
+----------------------]]

local disable_builtin_plugins = {
    "2html_plugin",
    "getscript",
    "getscriptPlugin",
    "gzip",
    "logipat",
    "netrw",
    "netrwPlugin",
    "netrwSettings",
    "netrwFileHandlers",
    "matchit",
    "tar",
    "tarPlugin",
    "rrhelper",
    "spellfile_plugin",
    "vimball",
    "vimballPlugin",
    "zip",
    "zipPlugin",
    "tutor",
    "rplugin",
    "syntax",
    "synmenu",
    "optwin",
    "compiler",
    "bugreport",
    "ftplugin"
}

local disable_builtin_providers = {
    "node",
    "perl",
    "ruby"
}

for _, plugin in pairs(disable_builtin_plugins) do
    vim.g['loaded_' .. plugin] = 1
end

for _, provider in ipairs(disable_builtin_providers) do
    vim.g[("loaded_" .. provider .. "_provider")] = 0
end

-- :help lua-loader
vim.loader.enable()

-- load plugins (they are stored at ~/.config/nvim/pack/start/plugins)
vim.cmd('packloadall')
vim.o.background = 'light'

local source = vim.fn.expand(os.getenv('HOME') .. '/.config/nvim/lua/')
local read = vim.fn.readdir(source)
for _, plugin in ipairs(read) do
    if plugin:match('%.lua$') then
        pcall(dofile, source .. plugin)
    end
end
