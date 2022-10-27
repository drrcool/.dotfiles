local status_ok, nightfox = pcall(require, 'nightfox')
if not status_ok then
  return
end

local M = {}

function M.setup()

  nightfox.setup({
    options = {
      transparent = true,
      terminal_colors = true,
      dim_inactive = true,
    }
  })
end

return M
