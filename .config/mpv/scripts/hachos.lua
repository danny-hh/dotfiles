local script_directory = debug.getinfo(1, 'S').source:sub(2):match('(.*/)')
package.path = script_directory .. '?.lua;' .. package.path

local mp = require 'mp'
local config = require('config')

for pattern, action in pairs(config.keymaps) do
    for k in pattern:gmatch('[^,]+') do
        local opt = k:match('[left|right|zZxXfF]')
        and { "repeatable" } or {}

        mp.add_forced_key_binding(k, function()
            mp.command(action)
        end, unpack(opt))
    end
end

for opt, arg in pairs(config.options) do
    mp.set_property('options/' .. opt:gsub('_', '-'), arg)
end

for _, file in pairs(config.scripts) do
    mp.commandv("load-script", os.getenv("HOME") .. file)
end
