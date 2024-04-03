return {
    {
        "folke/which-key.nvim",
        event = "VeryLazy",
        opts = {
            plugins = { spelling = true },
            defaults = {
                mode = { "n", "v" },
                ["<leader>g"] = { name = "+Git" },
                ["<leader>f"] = { name = "+Files" },
                ["<leader>l"] = { name = "+LSP" },
                ["<leader>q"] = { name = "+Quit/Session" },
                ["<leader>qq"] = { cmd = "<cmd>q<cr>", desc = "Quit" },
            },
        },
        config = function(_, opts)
            local wk = require("which-key")
            wk.setup(opts)
            wk.register(opts.defaults)
        end,
    },
}
