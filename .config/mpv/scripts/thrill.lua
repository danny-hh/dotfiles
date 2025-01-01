local mp = require('mp')
local has_ran = false

local extensions = {
    audio = {
       ".mp3", ".opus", ".flac"
    },

    image = {
       ".jpg", ".png", ".gif"
    },
}

local source = {
    audio = {
        "/home/irhl/.config/mpv/script-opts/piko.lua",
        "/home/irhl/.config/mpv/script-opts/typewriter.lua"
    },

    image = {
        "/home/irhl/.config/mpv/script-opts/imim.lua"
    },

    misc = {
        "/home/irhl/.config/mpv/script-opts/autoload.lua"
    }
}

function main()
    if has_ran then
        return
    end

    for _, script in ipairs(source.misc) do
        dofile(script)
    end

    local filename = mp.get_property("filename")
    for type, exts in pairs(extensions) do
        for _, ext in ipairs(exts) do
            if filename:match(ext.."$") then

                for _, script in ipairs(source[type]) do
                    mp.commandv("load-script", script)
                end

                has_ran = true
                return
            end
        end
    end
end

mp.register_event("start-file", main)
