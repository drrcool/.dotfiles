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
}
