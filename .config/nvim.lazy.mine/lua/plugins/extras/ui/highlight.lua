return {

	{
		"RRethy/vim-illuminate",
		event = "BufReadPre",
		opts = { delay = 200 },
		config = function(_, opts)
			require("illuminate").configure(opts)
		end,
	},
	{
		"m-demare/hlargs.nvim",
		event = "VeryLazy",
		opts = {
			color = "#ef9062",
			use_colorpalette = false,
			disable = function(_, bufnr)
				if vim.b.semantic_tokens then
					return true
				end
				local clients = vim.lsp.get_active_clients({ bufnr = bufnr })
				for _, c in pairs(clients) do
					local caps = c.server_capabilities
					if c.name ~= "null-ls" and caps.semanticTokenProvider and caps.semanticTokenProvider.full then
						vim.b.semantic_tokens = true
						return vim.b.semantic_tokens
					end
				end
			end,
		},
	},
	{
		"andymass/vim-matchup",
		lazy = false,
		enabled = true,
		init = function()
<<<<<<< HEAD:.config/nvim/lua/plugins/extras/ui/highlight.lua
			vim.g.matchup_matchparen_offscreen = { method = "popop" }
=======
			vim.g.matchup_matchparens_offscreen = { method = "popup" }
>>>>>>> 3f8a57c8413dec7ae47dcaf7baa3074029263a9d:.config/nvim.lazy.mine/lua/plugins/extras/ui/highlight.lua
		end,
	},
}
