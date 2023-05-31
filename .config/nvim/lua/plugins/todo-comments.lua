return {
	{
		"folke/todo-comments.nvim",
		cmd = { "TodoTrouble", "TodoTelescope" },
		event = "BufReadPost",
		config = true,
		keys = {
			{
				"]t",
				function()
					require("todo-comments").jump_next()
				end,
				desc = "Next Todo",
			},
			{
				"[t",
				function()
					require("todo-comments").jump_prev()
				end,
				desc = "Previous Todo",
			},
			{ "<leader>ct", "<cmd>TodoTrouble<cr>", desc = "ToDo (Trouble)" },
			{ "<leader>cT", "<cmd>TodoTelescope<cr>", desc = "Todo" },
		},
	},
}
