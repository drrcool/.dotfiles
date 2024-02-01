return {
  -- Add nightfox
  { "EdenEast/nightfox.nvim" },
  { "bluz71/vim-moonfly-colors", name = "moonfly", lazy = false, priority = 1000 },
  -- Configure LaxyVim to load the colorscheme we want
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "moonfly",
    },
  },
}
