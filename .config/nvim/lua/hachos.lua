local config = require('config')

for _, type in pairs{config.keymaps.silent, config.keymaps.standard} do
    for _, v in ipairs(type) do
        local arg = type == config.keymaps.silent and {noremap = true, silent = true} or nil
        vim.keymap.set(v[1], v[2], v[3], arg)
    end
end

for v1, v2 in pairs(config.options) do
    vim.opt[v1] = v2
end

for i = 1, #config.theme, 3 do
    local hl, fg, bg = config.theme[i], config.theme[i + 1], config.theme[i + 2]
    local bold = (hl == "left" or hl == "right") and true or false

    vim.api.nvim_set_hl(0, hl, { fg = fg, bg = bg, bold = bold })
end

_G.git = function()
    local url_full = 'https://github.com/'
    local url_raw = 'https://raw.githubusercontent.com/'

    local dir = vim.fn.stdpath('config') .. '/pack/plugins/start'
    local skk = dir .. '/skk'
    vim.fn.mkdir(dir,  'p')
    vim.fn.mkdir(skk, 'p')

    local reply = {}
    for _, repo in ipairs(config.source_full) do
        local a = repo:match('.*/(.*)')
        local b = dir .. '/' .. a
        if vim.fn.isdirectory(b) == 0 then
            table.insert(reply, 'git clone ' .. url_full .. repo .. '.git ' .. b)
        end
    end
    for _, repo in ipairs(config.source_raw) do
        local a = repo:match('.*/(.*)')
        local b = skk .. '/' .. a
        if vim.fn.filereadable(b) == 0 then
            table.insert(reply, 'curl -o ' .. b .. ' ' .. url_raw .. repo)
        end
    end
    os.execute(table.concat(reply, ' && '))

    io.write("\27[31m", "download complete!", "\27[0m")
    io.flush()
end
