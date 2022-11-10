local M = {}

M.setup = function()
  require('noice').setup({
    lsp = {
      hover = {
        enabled = false
      }
    }
  })
end

return M
