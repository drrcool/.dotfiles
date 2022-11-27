require("user.options")
require("user.keymaps")
require("user.plugins")
require('user.lsp')
require("user.autocommands")
vim.cmd("colorscheme nightfox")

require('nightfox').setup({
  options = {
    transparent = true,
    dim_inactive= true,
    styles = {
      comments = "italic",
      functions = "italic",
      keywords = "italic",
      strings = "NONE",
      variables = "NONE",
      types = "italic,bold"},
      inverse={
        match_paren = true,
          visual = true,
        }, -- inverse the colorscheme
      }
})
