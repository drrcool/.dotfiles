return {
	"nvim-telescope/telescope.nvim",
	dependencies = {
		{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
		"nvim-telescope/telescope-file-browser.nvim",
		"nvim-telescope/telescope-project.nvim",
		"ahmedkhalf/project.nvim",
		"cljoly/telescope-repo.nvim",
	},
	cmd = "Telescope",
	keys = {
		{ "<leader><space>", "<cmd>Telescope find_files<cr>", desc = "Find Files" },
		{ "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find Files" },
		{ "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Recent" },
		{ "<leader>fb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
		{ "<leader>fg", "<cmd>Telescope git_files<cr>", desc = "Git Files" },
		{ "<leader>f/", "<cmd>Telescope live_grep<cr>", desc = "Grep" },
		{
			"<leader>zc",
			function()
				require("telescope.builtin").colorscheme({ enable_preview = true })
			end,
			desc = "Colorscheme",
		},
	},
	config = true,
}
