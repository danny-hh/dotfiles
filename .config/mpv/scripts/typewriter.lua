local mp = require('mp')

extensions_audio = {'flac', 'mp3', 'opus'}

function file_extension(path)
    return string.match(table.concat(extensions_audio),
    path:match('%.([^%.]+)$')) and true or false
end

function main()
    local path = mp.get_property('path')
    if path and not file_extension(path) then
        return
    end

    local output = os.getenv('HOME') .. '/.config/mpv/typewriter.csv'
    local size_limit = 10 * 1024 * 1024 -- 10mb

    local file = io.open(output, 'rb')
    if file then
        local size = file:seek('end')
        file:close()

        if size > size_limit then
            io.open(output, 'w'):close()
        end
    end

    file = io.open(output, 'a')
    if file then
        local current_date = os.date('[%Y-%m-%d_%H:%M:%S]')
        local metadata = mp.get_property_native('metadata')
        file:write('\n' .. current_date .. '\n')

        if metadata then
            for key, value in pairs(metadata) do
                file:write(key .. ': ' .. value .. '\n')
            end
        end

        file:close()
    end
end

mp.register_event('file-loaded', main)
