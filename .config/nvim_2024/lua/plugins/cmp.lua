return {
    -- Setup Copilot
    {
        "zbirenbaum/copilot.lua",
        cmd = "Copilot",
        event = "InsertEnter",
        config = function()
            require("copilot").setup({
            })
        end,
    },
    {
        "zbirenbaum/copilot-cmp", 
        config = function()
            require('copilot_cmp').setup()
        end
    },
    {
        "hrsh7th/nvim-cmp",
        event = "InsertEnter",
        dependencies = {
            "hrsh7th/cmp-nvim-lsp",
            "saadparwaiz1/cmp_luasnip",
        },
        config = function()
            local cmp = require("cmp")
            local luasnip = require("luasnip")
            local compare = require("cmp.config.compare")

            local has_words_before = function()
                if vim.api.nvim_buf_get_option(0, "buftype") == "prompt" then return false end
                local line, col = unpack(vim.api.nvim_win_get_cursor(0))
                return col ~= 0 and vim.api.nvim_buf_get_text(0, line-1, 0, line-1, col, {})[1]:match("^%s*$") == nil
            end

            cmp.setup({
                completion = {
                    completeopt = "menu,menuone,noinsert",
                },
                sorting = {
                    priority_weight = 2,
                    comparators = {
                        require("copilot_cmp.comparators").prioritize,
                        compare.score,
                        compare.recently_used,
                        compare.offset,
                        compare.exact,
                        compare.kind, 
                        compare.sort_text,
                        compare.length,
                        compare.order
                    },
                },
                snippet = {
                    expand = function(args)
                        require("luasnip").lsp_extend(args.body)
                    end,
                },
                mapping = cmp.mapping.preset.insert({
                    ["<C-e>"] = cmp.mapping.abort(),
                    ["<CR>"] = cmp.mapping({
                        i = cmp.mapping.confirm({behavior=cmp.ConfirmBehavior.Replace, select = false}),
                        c = function(fallback)
                            if cmp.visible() then
                                cmp.confirm({behavior=cmp.ConfirmBehavior.Replace, select=false})
                            else
                                fallback()
                            end
                        end,
                    }),
                    ["<Tab>"] = vim.schedule_wrap(function(fallback)
                        if cmp.visible() and has_words_before() then
                            cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
                        else
                            fallback()
                        end
                    end)
                }),
                sources = cmp.config.sources({
                    { name = "nvim_lsp_signature_help", group_index = 1},
                    { name = "nvim_lsp", group_index=1},
                    { name = "luasnip", group_index=1},
                    { name = "copilot", group_index=2}
                })
            })
        end
    },
    {
        "L3MON4D3/LuaSnip",
        dependencies={
            {
                "rafamadriz/friendly-snippets",
                config = function()
                    require("luasnip.loaders.from_vscode").lazy_load()
                end,
            },
        },
        build = "make install_jsregexp",
        opts = {
            history = true,
            delete_check_events = "TextChanged"
        },
        keys = {
            {
                "<C-j>",
                function()
                    return require("luasnip").jumpable(1) and "<Plug>luasnip-jump-next" or "<C-j>"
                end,
                expr = true, remap = true, silent = true, mode = "i",
            },
            { "<C-j>", function() require("luasnip").jump(1) end, mode = "s"}, 
            { "<C-k>", function() require("luasnip").jump(-1) end, mode = {"i","s"}},
        },
        config = function(_, opts)
            require("luasnip").setup(opts)
        end,
    }
}

