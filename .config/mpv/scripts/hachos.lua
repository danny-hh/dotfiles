local script_directory = debug.getinfo(1, 'S').source:sub(2):match('(.*/)')
package.path = script_directory .. '?.lua;' .. package.path

local mp = require 'mp'
local config = require('config')

for pattern, action in pairs(config.keymaps) do
    local function keymatch(k)
        return k:match('[left|right|zZxXfF|hjkl]') and "repeatable" or nil
    end

    for k in pattern:gmatch('[^,]+') do
        local function action_handle()
            (type(action) == "function" and action or function() mp.command(action) end)()
        end
        mp.add_forced_key_binding(k, action_handle, keymatch(k))
    end
end

for opt, arg in pairs(config.options) do
    mp.set_property('options/' .. opt:gsub('_', '-'), arg)
end
