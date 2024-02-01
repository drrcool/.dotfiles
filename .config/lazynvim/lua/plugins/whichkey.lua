return {
  {
    "folke/which-key.nvim",
    opts = {
      plugins = { spelling = true },
      defaults = {
        mode = { "n", "v" },
        ["g"] = { name = "+goto" },
        ["gs"] = { name = "+surround" },
        ["]"] = { name = "+next" },
        ["["] = { name = "+prev" },
        ["<leader><tab>"] = { name = "+tabs" },
        ["<leader>b"] = { name = "+buffer" },
        ["<leader>c"] = { name = "+code" },
        ["<leader>f"] = { name = "+file/find" },
        ["<leader>g"] = { name = "+git" },
        ["<leader>gh"] = { name = "+hunks" },
        ["<leader>q"] = { name = "+quit/session" },
        ["<leader>s"] = { name = "+search" },
        ["<leader>u"] = { name = "+ui" },

        ["<leader>w"] = { name = "Windows" },
        ["<leader>ws"] = { "<C-w>s", "Split Below" },
        ["<leader>wv"] = { "<C-w>v", "Split Right" },
        ["<leader>wh"] = { "<C-w>h", "Move Left" },
        ["<leader>wl"] = { "<C-w>l", "Move Right" },
        ["<leader>wj"] = { "<C-w>j", "Move Down" },
        ["<leader>wk"] = { "<C-w>k", "Move Up" },

        ["<leader>x"] = { name = "Diagnostics" },
      },
    },
  },
}
