local luasnip = require("luasnip")
local cmp = require("cmp")

local select_opts = { behavior = cmp.SelectBehavior.Select }

cmp.setup({
    mapping = {
        ['<Up>']   = cmp.mapping.select_prev_item(select_opts),
        ['<Down>'] = cmp.mapping.select_next_item(select_opts),
        ['<Tab>']  = cmp.mapping.confirm(select_opts),
        ["<C-e>"]  = cmp.mapping.scroll_docs(4),
        ["<C-z>"]  = cmp.mapping.scroll_docs(-4),
        ["<C-q>"]  = cmp.mapping.abort(),
        ["<C-f>"]  = cmp.mapping.close(),
    },
    completion = {
        completeopt = "menu,menuone,noinsert"
    },
    snippet = {
        expand = function(args)
            luasnip.lsp_expand(args.body)
        end
    },
    sources = {
        { name = "luasnip",  keyword_length = 2 },
        { name = "nvim_lua", keyword_length = 2 },
        { name = "nvim_lsp", keyword_length = 1 },
        { name = "buffer",   keyword_length = 3 },
        { name = "path",     keyword_length = 6 }
    },
    formatting = {
        fields = { "menu", "abbr", "kind" },
        format = function(entry, item)
            local menu_icon = {
                luasnip  =  "⋗",
                nvim_lua =  " ",
                nvim_lsp =  "λ",
                buffer   =  "Ω",
                path     =  "🖫"
            }
            item.menu = menu_icon[entry.source.name]
            return item
        end
    },
    window = {
        documentation = {
            border = "rounded",
            winhighlight = "Normal:CmpMaster," ..
                           "String:CmpDefault," ..
                           "Identifier:CmpDefault," ..
                           "Special:CmpSpecial," ..
                           "Operator:CmpDefault," ..
                           "Float:CmpDefault," ..
                           "Number:CmpDefault," ..
                           "Underlined:CmpSpecial",
                           "Statement:CmpSpecial",
                           "NonText:CmpSpecial",
                           "Title:CmpSpecial",
                           "TODO:CmpSpecial",
        },
        completion = {
            scrollbar = false,
            border = "rounded",
            winhighlight = "Normal:CmpDefault," ..
	                   "Special:CmpDefault"
        }
    }
})
