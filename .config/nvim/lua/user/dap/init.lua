local M = {}


M.setup_dap = function()
  local status_ok, dap = pcall(require, 'dap')
  if not status_ok then
    return
  end
  local status_icon_ok, icons = pcall(require, 'user.icons')
  if not status_icon_ok then
    return
  end


  vim.fn.sign_define("DapBreakpoint", { text = icons.ui.Bug, texthl = "DiagnosticSignError", linehl = "", numhl = "" })
  vim.fn.sign_define("DapStopped", { text = icons.misc.Robot, texthl = "", linehl = "", numhl = "" })



end



M.setup_ui = function()
  local status_ok, dap = pcall(require, "dap")
  if not status_ok then
    return
  end
  local dapui = require "dapui"
  dapui.setup {
    expand_lines = true,
    icons = { expanded = "", collapsed = "", circular = "" },
    mappings = {
      -- Use a table to apply multiple mappings
      expand = { "<CR>", "<2-LeftMouse>" },
      open = "o",
      remove = "d",
      edit = "e",
      repl = "r",
      toggle = "t",
    },
    layouts = {
      {
        elements = {
          { id = "scopes", size = 0.33 },
          { id = "breakpoints", size = 0.17 },
          { id = "stacks", size = 0.25 },
          { id = "watches", size = 0.25 },
        },
        size = 0.33,
        position = "right",
      },
      {
        elements = {
          { id = "repl", size = 0.45 },
          { id = "console", size = 0.55 },
        },
        size = 0.27,
        position = "bottom",
      },
    },
    floating = {
      max_height = 0.9,
      max_width = 0.5, -- Floats will be treated as percentage of your screen.
      border = vim.g.border_chars, -- Border style. Can be 'single', 'double' or 'rounded'
      mappings = {
        close = { "q", "<Esc>" },
      },
    },
  }

  dap.listeners.after.event_initialized["dapui_config"] = function()
    dapui.open()
  end
end

M.setup_other = function()
  require("nvim-dap-virtual-text").setup {
    commented = true,
  }
end

local function configure_debuggers()
  require("user.dap.lua").setup()
  require("user.dap.python").setup()
  require("user.dap.javascript").setup()
  --[[ require("user.dap.typescript").setup() ]]
end

function M.setup()
  M.setup_dap() -- Configuration
  M.setup_ui() -- Extensions
  M.setup_other()
  configure_debuggers() -- Debugger
  require("user.dap.keymaps").setup() -- Keymaps
end

configure_debuggers()

return M
