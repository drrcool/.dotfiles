local M = {}

function M.setup()
  require("package-info").setup({
    colors = {
      up_to_date = "#113001",
      enable = true,
      outdated = "#ff6000",
    }, -- highlight groups for the different package states
    package_manager = "npm", -- npm, yarn or pnpm
  })
end

return M
