local M = {}

local nls = require "null-ls"
local nls_utils = require "null-ls.utils"
local b = nls.builtins
local with_diagnostics_code = function(builtin)
  return builtin.with {
    diagnostics_format = "#{m} [#{c}]",
  }
end
-- local refurb = require "config.lsp.null-ls.diagnostics.refurb"

-- local with_root_file = function(builtin, file)
--   return builtin.with {
--     condition = function(utils)
--       return utils.root_has_file(file)
--     end,
--   }
-- end
require('user.lsp.null-ls.eslint')
require('user.lsp.null-ls.prettier')
local sources = {
  -- formatting
  b.formatting.fixjson,
  b.formatting.stylua,
  b.hover.dictionary,
}

function M.setup(client, bufnr)
  nls.setup {
    debug = false,
    debounce = 150,
    save_after_format = false,
    sources = sources,
    on_attach = function(client, bufnr)
      if client.server_capabilities.documentFormattingProvier then
        vim.cmd("nnoremap <silent><buffer> <leader>lf :lua vim.lsp.buf.formatting()<CR>")

        --format on save´˜
        vim.cmd("autocmd BufWritePost <buffer> lua vim.lsp.buf.formatting()")
      end
      if client.server_capabilities.documentRangeFormattingProvider then
        vim.cmd("xnoremap <silent><buffer> <leader>lf :lua vim.lsp.bug.range_formatting({})<CR>")
      end
    end,
    root_dir = nls_utils.root_pattern ".git",
  }
end

return M
