-- after downloading the plugins, mason should automatically
-- download the ensured servers list, if you need assistance
-- then mason logs will show.
local lspconfig = require('lspconfig')
local mason = require('mason')
local mason_lspconfig  = require('mason-lspconfig')
local cmp_nvim_lsp = require('cmp_nvim_lsp')
local lsp_capabilities = cmp_nvim_lsp.default_capabilities()

local servers = {
    'lua_ls',
}

mason.setup()
mason_lspconfig.setup({
    ensure_installed = servers
})

local function on_attach(client, bufnr)
    local function buf_set_keymap(...)
        vim.api.nvim_buf_set_keymap(bufnr, ...)
    end
    local opts = {noremap = true, silent = true}

    buf_set_keymap('n', 'A', '<cmd> lua vim.lsp.buf.hover()       <cr>', opts)
    buf_set_keymap('n', 'R', '<cmd> lua vim.lsp.buf.rename()      <cr>', opts)
    buf_set_keymap('n', 'C', '<cmd> lua vim.lsp.buf.code_action() <cr>', opts)
    buf_set_keymap('n', 'D', '<cmd> lua vim.lsp.buf.references()  <cr>', opts)
end

for _, lsp in ipairs(servers) do
    local flags = {
        debounce_text_changes =  150
    }

    local settings = {
        Lua = {
            diagnostics = {
                globals = {'vim'},
                disable = {'lowercase-global', 'trailing-space'}
            },
            telemetry = {
                enable = false
            }
        }
    }

    lspconfig[lsp].setup({
        settings = settings,
        flags = flags,
        on_attach = on_attach,
        capabilities = lsp_capabilities
    })
end
