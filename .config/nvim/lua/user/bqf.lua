local function toggleQf()
  local ft = vim.bo.filetype
  if ft == 'qf' then
    vim.cmd('cclose')
  else
    vim.cmd('copen')
  end
end

local M = {}

M.setup = function()

  vim.keymap.set("n", "<leader>q", toggleQf, {})
  require('bqf').setup({
    preview = {
      border_chars = { "│", "│", "─", "─", "╭", "╮", "╰", "╯", "│" },
    },
    filter = {
      fzf = {
        action_for = {
          ["enter"] = "signtoggle",
        },
      },
    },
  })

end

return M
