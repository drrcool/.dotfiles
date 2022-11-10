local cmp_nvim_lsp_status, cmp_nvim_lsp = pcall(require, 'cmp_nvim_lsp')
local mason_status, mason = pcall(require, "mason")
local mason_lspconfig_status, mason_lspconfig = pcall(require, 'mason-lspconfig')
local lsp_format_status, lsp_format = pcall(require, 'lsp-format')
local mason_null_ls_status, mason_null_ls = pcall(require, 'mason-null-ls')
local mason_update_status, mason_update_all = pcall(require, 'mason-update-all')
if not (mason_null_ls_status and cmp_nvim_lsp_status and mason_status and mason_lspconfig_status and lsp_format_status) then

  print("One of Mason, Mason LSP Config, Completion or LSP Format is not installed")
  return
end

lsp_format.setup({
  order = {
    'tsserver',
    'prettier',
    'eslint',
  }
})

-- After the LSOP attached set key map
local on_attach = function(client, bufnr)
  local function buf_set_option(...)
    vim.api.nvim_buf_set_option(bufnr, ...)
  end

  require('user.lsp.null-ls.formatters').setup(client, bufnr)
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  client.config.flags.debounce_text_changes = 300

  client.server_capabilities.documentFormattingProvider = true

  if (client.name ~= 'tsserver') then
    lsp_format.on_attach(client)
  end
end
local normal_capabilities = vim.lsp.protocol.make_client_capabilities()

normal_capabilities.textDocument.foldingRange = {
  dynamicRegistration = false,
  lineFoldingOnly = true
}
local capabilities = cmp_nvim_lsp.default_capabilities(normal_capabilities)

-- We then iterate over their names and load their relevant
-- configuration files, which are stored in lua/lsp/servers,
-- passing along the global on_attach and capabilities functions
local servers = {
  "sumneko_lua",
  "tsserver",
  "eslint",
}

require("user.lsp.cmp")
mason_lspconfig.setup({ ensure_installed = servers, automatic_installation = true })
mason_null_ls.setup({ ensure_installed = { 'prettier', 'codespell', 'sql_formatter', 'black', 'stylua' } })
mason_update_all.setup()
mason.setup({})

-- Setup each server
for _, s in pairs(servers) do
  local server_config_ok, mod = pcall(require, "user.lsp.servers." .. s
  )
  if not server_config_ok then
    print("The LSP '" .. s .. "' does not have a config.")
  else
    mod.setup(on_attach, capabilities)
  end
end

-- Global diagnostic settings
vim.diagnostic.config({
  virtual_text = true,
  severity_sort = true,
  update_in_insert = false,
  float = {
    header = "",
    source = "always",
    border = "rounded",
    focusable = true,
  },
})
-- Removing errors from gutter for now, trying inline errors instead

-- Change Error Signs in Gutter
local signs = { Error = "✘", Warn = " ", Hint = "", Info = " " }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  -- vim.fn.sign_define(hl, { text = icon })
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end
