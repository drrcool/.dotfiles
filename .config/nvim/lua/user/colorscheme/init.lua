local M = {}


M.applyColorScheme = function(color)

  if (color == 'nightfox') then
    require('user.colorscheme.nightfox')
  elseif (color == 'tokyonight') then
    require('user.colorscheme.tokyonight')
  elseif (color == 'catppuccin') then
    require('user.colorscheme.catppuccin')
  else
    vim.cmd('colorscheme ' .. color)
  end
end
return M
