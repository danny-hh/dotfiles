-- trim trailing whitespaces
vim.api.nvim_create_autocmd({ 'BufWritePre' }, {
    pattern = { "*" },
    command = [[%s/\s\+$//e]],
})

-- forbidden file names
-- https://github.com/dylanaraps/dotfiles/blob/master/vim/vimrc#L68
vim.api.nvim_create_autocmd({'BufWritePre'}, {
  pattern = {'*'},
  callback = function()
    if string.match(vim.fn.expand("<afile>"), "[:;]") then
      vim.api.nvim_err_writeln("Forbidden file name: " .. vim.fn.expand("<afile>"))
      vim.api.nvim_command("silent! :bw!")
    end
  end
})

-- automatically create non-existing parent directories when saving
-- https://github.com/classabbyamp/dotfiles/blob/master/dot_config/nvim/init.lua#L149-L156
vim.api.nvim_create_autocmd({'BufWritePre', 'FileWritePre'}, {
    callback = function(ev)
        if ev.match:match("^%w%w+://") then return end
        local file = vim.loop.fs_realpath(ev.match) or ev.match
        vim.fn.mkdir(vim.fn.fnamemodify(file, ":p:h"), "p")
    end
})

-- restore cursor position when opening a file
-- https://github.com/farmergreg/vim-lastplace/issues/28#issuecomment-1336129506
vim.api.nvim_create_autocmd("BufRead", {
  callback = function(opts)
    vim.api.nvim_create_autocmd("FileType", {
      once = true,
      buffer = opts.buf,
      callback = function()
        local ft = vim.bo[opts.buf].ft
        local last_pos = vim.api.nvim_buf_get_mark(opts.buf, '"')
        if
          not (ft:match("commit") and ft:match("rebase"))
          and last_pos[1] > 1
          and last_pos[1] <= vim.api.nvim_buf_line_count(opts.buf)
        then
          vim.api.nvim_feedkeys('g`"', "x", false)
        end
      end,
    })
  end,
})

-- window navigation wrap around in Vim
-- https://stackoverflow.com/a/73612761
-- |----------|----------|-----------|
-- |          |          |           |
-- | NERDTree |   File1  |   File2   |
-- |          |          |           |
-- |          |----------|-----------|
-- |          |          |           |
-- |          |   File3  |   File4   |
-- |          |          |           |
-- |----------|----------|-----------|

local function try_jump_window(direction, count)
  local prev_win_nr = vim.fn.winnr()
  vim.cmd(count .. "wincmd " .. direction)
  return vim.fn.winnr() ~= prev_win_nr
end

local function jump_window_with_wrap(direction, opposite)
  return function ()
    if not try_jump_window(direction, vim.v.count1) then
      try_jump_window(opposite, 999)
    end
  end
end

local opts = { silent = true, noremap = true }
vim.keymap.set("n", "<C-w>h", jump_window_with_wrap("h", "l"), opts)
vim.keymap.set("n", "<C-w>l", jump_window_with_wrap("l", "h"), opts)
vim.keymap.set("n", "<C-w>j", jump_window_with_wrap("j", "k"), opts)
vim.keymap.set("n", "<C-w>k", jump_window_with_wrap("k", "j"), opts)

-- git branch indicator for statusline
--          +-----------------------------------------------------------+
-- PREVIEW: | config.lua:lua {main} [i][+]           Ln 85, Col 47 [2/5]|
--          +-----------------------------------------------------------+
function git_info()
    -- current buffer directory
    local bufname = vim.fn.expand('%:p:h')

    -- current directory change to buffer's directory
    vim.fn.chdir(bufname)

    -- get git branch
    local result = vim.fn.systemlist("git branch --show-current 2>/dev/null")
    if result and #result > 0 and result[1] then
        vim.b.git_branch = result[1]
    else
        vim.b.git_branch = 'b?'
    end

    -- current directory change to home directory
    vim.fn.chdir(vim.fn.expand('$HOME'))
end

-- update git branch
vim.api.nvim_command('autocmd BufEnter,BufWinEnter,WinEnter * lua git_info()')
