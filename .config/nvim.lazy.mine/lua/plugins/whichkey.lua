return {
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		config = function()
			local wk = require("which-key")
			wk.setup({
				show_help = false,
				plugins = { spelling = true },
				key_labels = { ["<leader>"] = "SPC" },
				triggers = "auto",
			})
			wk.register({
				w = { "<cmd>update!<CR>", "Save" },
				q = { "<cmd>quit<CR>", "Quit" },
				h = { name = "+Help" },
				b = { name = "+Buffer" },
				d = { name = "+Debug" },
				j = { name = "+Jump" },
				n = { name = "+Notes" },
				t = { name = "+Test" },
				z = { name = "+System" },
				p = { name = "+Project" },
				v = { name = "+View" },
				["sn"] = { name = "+Noice" },
				f = { name = "+File" },
				g = { name = "+Git" },
				s = { name = "+Search" },
				c = {
					name = "+Code",
					x = {
						name = "Swap Next",
						f = "Function",
						p = "Parameter",
						c = "Class",
					},
					X = {
						name = "Swap Previous",
						f = "Function",
						p = "Parameter",
						c = "Class",
					},
				},
			}, { prefix = "<leader>" })
		end,
	},
	{
		"mrjones2014/legendary.nvim",
		keys = {
			{ "<C-S-p>", "<cmd>Legendary<cr>", desc = "Legnedary" },
			{ "<leader>hl", "<cmd>Legendary<cr>", desc = "Legendary" },
		},
		opts = {
			which_key = { auto_register = true },
		},
	},
}
