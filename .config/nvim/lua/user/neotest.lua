local status_ok, neotest = pcall(require, "neotest")
if not status_ok then
  return
end
local function config_test()
  vim.api.nvim_exec(
    [[
        " Test config
        let test#strategy = "neovim"
        let test#neovim#term_position = "belowright"
        let g:test#preserve_screen = 1
        " Python
        let test#python#runner = 'pyunit'
        " let test#python#runner = 'pytest'
        " Javascript
        let test#javascript#reactscripts#options = "--watchAll=false"
        let g:test#javascript#runner = 'newt exec npx jest'
        let g:test#javascript#cypress#executable = 'npx cypress run-ct'
        " csharp
        let test#csharp#runner = 'dotnettest'
    ]],
    false
  )
end

neotest.setup({
  adapters = {
    require "neotest-python" {
      dap = { justMyCode = false },
      runner = 'unittest'
    },
    require "neotest-jest",


  },
  consumers = {
    overseer = require 'neotest.consumers.overseer'
  },
  overseer = {
    enabled = true,
    force_default = true,
  },

  config_test()
})
