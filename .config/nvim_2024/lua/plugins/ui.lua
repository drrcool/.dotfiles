return {
    {
        "nvim-tree/nvim-web-devicons",
        dependencies = { "DaikyXendo/nvim-material-icon" },
        config = function ()
            require("nvim-web-devicons").setup({
                override = require("nvim-material-icon").get_icons(),
            })
        end,
    },
    {
        "folke/tokyonight.nvim",
        lazy = false,
        priority = 1000,
        opts = {
            style = "night",
            transparent = false,
            styles = {
                sidebars = "transparent",
                floats = "transparent",
            },
        },
        config = function(_, opts)
            local tokyonight = require("tokyonight")
            tokyonight.setup(opts)
            tokyonight.load()
        end,
    },
    {
        "catppuccin/nvim", lazy=false, name="catppuccin"
    },
    {
        "stevearc/dressing.nvim",
        event = "VeryLazy",
        opts = {},
    },
    {
        "folke/noice.nvim",
        dependencies = { 
            "MunifTanjim/nui.nvim",
            "rcarriga/nvim-notify",
        },
        event = "VeryLazy",
        enable = true,
        opts = {
            override = {
                ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
                ["vim.lsp.util.stylize_markdown"] = true,
                ["cmp.entry.get_documentation"] = true

            },
        },
        presets = {
            bottom_search = true,
            command_palette = true,
            long_message_to_split = true,
            inc_rename = true,
            lsp_doc_border = true
        },

    }
}
            
