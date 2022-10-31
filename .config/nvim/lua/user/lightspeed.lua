local M = {}


function M.setup()
  require('lightspeed').setup {
    just_to_unique_chars = false,
    safe_labels = {}
  }
end

return M
