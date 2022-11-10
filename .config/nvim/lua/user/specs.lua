local M = {}
M.setup = function()

  require('specs').setup {
    popup = {
      fader = require('specs').sinus_fader,
      resizer = require('specs').slide_resizer
    }
  }

end

return M
