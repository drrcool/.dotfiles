return {
	{
		"nvim-telescope/telescope.nvim",
		dependencies = {
			{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
		},
		cmd = "Telescope",
    -- stylua: ignore
    keys = {
      { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find Files" },
      { "<leader>fg", "<cmd>Telescope git_files<cr>", desc = "Git Files" },
      { "<leader>fb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
      { "<leader>fh", "<cmd>Telescope help_tags<cr>", desc = "Help" },
    },
		opts = {
			defaults = {
				mappings = {
					i = {
						["<C-j>"] = function(...)
							require("telescope.actions").move_selection_next(...)
						end,
						["<C-k>"] = function(...)
							require("telescope.actions").move_selection_previous(...)
						end,
						["<C-n>"] = function(...)
							require("telescope.actions").cycle_history_next(...)
						end,
						["<C-p>"] = function(...)
							require("telescope.actions").cycle_history_prev(...)
						end,
					},
				},
			},
		},
		config = function(_, opts)
			local telescope = require("telescope")
			telescope.setup(opts)
			telescope.load_extension("fzf")
		end,
	},
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		opts = {
			plugins = { spelling = true },
			defaults = {
				mode = { "n", "v" },
				["<leader>f"] = { name = "+File" },
				["<leader>w"] = { name = "+Window" },
				["<leader>ws"] = { cmd = "<C-w>s", desc = "Split Vertically" },
				["<leader>wv"] = { cmd = "<C-w>v", desc = "Split Horizontally" },
				["<leader>wh"] = { cmd = "<C-w>h", desc = "Move Left" },
				["<leader>wj"] = { cmd = "<C-w>j", desc = "Move Down" },
				["<leader>wk"] = { cmd = "<C-w>k", desc = "Move Up" },
				["<leader>wl"] = { cmd = "<C-w>l", desc = "Mode Down" },
				["<leader>q"] = { name = "+Quit/Session" },
				["<leader>qq"] = { cmd = "<cmd>q<cr>", desc = "Quit" },
				["<leader>z"] = { name = "+System" },
				["<leader>zl"] = { cmd = "<cmd>Lazy<cr>", desc = "Lazy" },
			},
		},
		config = function(_, opts)
			local wk = require("which-key")
			wk.setup(opts)
			wk.register(opts.defaults)
		end,
	},
}
