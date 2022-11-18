require('catppuccin').setup({
  flavour = 'mocha',
  transparent_background = false,
  term_colors = true,
  dim_inactive = {
    enable = true,
    shade = 'dark',
    percentage = 0.15
  },
  styles = {
    comments = { 'italic' },
    conditionals = { 'italic' },
  }

})
vim.cmd('colorscheme catppuccin')

