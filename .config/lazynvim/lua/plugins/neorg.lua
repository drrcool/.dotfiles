return {
  "nvim-neorg/neorg",
  build = ":Neorg sync-parsers",
  opts = {
    load = {
      ["core.defaults"] = {},
      ["core.norg.concealer"] = {},
      ["core.norg.dirman"] = {
        config = {
          workspaces = {
            notes = "~/notes",
          },
        },
      },
    },
  },
  dependencies = { { "nvim-lua/plenary.nvim" } },
  keys = {
    { "<leader>nw", "<cmd>Neorg workspaces notes<cr>", desc = "Open Notes" },
    { "<leader>nn", "<cmd>core.norg.dirman.new.note<cr>", desc = "New Note" },
  },
}
