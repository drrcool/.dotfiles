return {

	{
		"folke/styler.nvim",
		event = "VeryLazy",
		config = function()
			require("styler").setup({
				themes = {
					markdown = { colorscheme = "catppuccin" },
					help = { colorscheme = "gruvbox" },
				},
			})
		end,
	},
	{
		"folke/tokyonight.nvim",
		lazy = false,
		priority = 1000,
		config = function()
			local tokyonight = require("tokyonight")
			tokyonight.setup({ style = "storm" })
		end,
	},
	{
		"catppuccin/nvim",
		lazy = false,
		name = "catppuccin",
		priority = 1000,
		config = function()
			vim.cmd("colorscheme catppuccin")
		end,
	},
	{
		"ellisonleao/gruvbox.nvim",
		lazy = true,
		priority = 1000,
		config = function()
			require("gruvbox").setup()
		end,
	},
	{
		"EdenEast/nightfox.nvim",
		lazy = true,
		priority = 1000,
		config = function()
			require("nightfox").setup({
				options = {
					transparent = true,
					dim_inactive = true,
					terminal_colors = true,
				},
			})
		end,
	},
}
