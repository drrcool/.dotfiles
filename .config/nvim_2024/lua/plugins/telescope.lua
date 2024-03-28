return {
    {
        "nvim-telescope/telescope.nvim",
        dependencies={
            {"nvim-telescope/telescope-fzf-native.nvim", build="make"},
            "nvim-lua/plenary.nvim"
        },
        cmd = "Telescope",
        keys = {
            { "<leader>ff", "<cmd>Telescope find_files<cr>", desc="Find Files"},
            { "<leader>fg", "<cmd>Telescope git_files<cr>", desc="Git Files"},
            { "<leader>fb", "<cmd>Telescope buffers<cr>", desc="Buffers"},
            { "<leader>fh", "<cmd>Telescope help_tags<cr>", desc="Help"}
        },
        config = function(_, opts)
            local telescope = require "telescope"
            telescope.setup(opts)
            telescope.load_extension("fzf")
        end,
    }
}
