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
				w = { name = "+Window" },
				n = { name = "+Notes" },
				p = { name = "+Project" },
				z = { name = "+System" },
				b = { name = "+Buffer" },
				q = { "<cmd>lua require('util').smart_quit()<CR>", "Quit" },
				f = { name = "+File" },
				g = { name = "+Git" },
				h = { name = "+Help" },
				j = { name = "+Jump" },
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
			{ "<C-S-p>", "<cmd>Legendary<cr>", desc = "Legendary" },
		},
		opts = {
			which_key = { auto_register = true },
		},
	},
}
