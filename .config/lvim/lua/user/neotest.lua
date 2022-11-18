local status_ok, neotest = pcall(require, "neotest")
if not status_ok then
  return
end

neotest.setup({
  adapters = {
    require "neotest-python" {
      dap = { justMyCode = false },
      runner = 'unittest'
    },
    require("neotest-jest")({
      jestCommand = "newt exec yarn run test",
    })


  },
  consumers = {
    overseer = require 'neotest.consumers.overseer'
  },
  overseer = {
    enabled = true,
    force_default = true,
  },

})
