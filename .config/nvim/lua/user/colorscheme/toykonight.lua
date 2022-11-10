
  local status_ok, tokyo = pcall(require, 'tokyonight')
  if not status_ok then
    return
  end

  tokyo.setup({
    style = 'night',
    transparent = true,
    dim_inactive = true,
    terminal_colors = true,
    styles = {
      comments = { italic = true },
      keywords = { italic = true },

    },
    lualine_bold = true

  })
  vim.cmd('colorscheme tokyonight')
