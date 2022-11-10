local M = {}
M.setup = function()
require('luatab').setup({
  separator = function()
    return ""
  end
})
end

return M
