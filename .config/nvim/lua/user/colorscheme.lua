local color = 'nightfox'

if (color == "nightfox") then
  local status_ok, nightfox = pcall(require, 'nightfox')
  if not status_ok then
    return
  end

  nightfox.setup({
    options = {
      transparent = true,
      dim_inactive = true,
      terminal_colors = true,

    }
  })
  vim.cmd('colorscheme carbonfox')
end


if (color == 'tokyonight') then

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
end
