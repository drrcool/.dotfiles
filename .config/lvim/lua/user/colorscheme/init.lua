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

M.randomSchemeName = function()
  local options = {
    'nvcode',
    'onedark',
    'palenight',
    'snazzy',
    'nvim-hybrid',
    'space-vim',
    'darker',
    'dark ocean',
    'oceanic',
    'codedark',
    'nightfly',
    'darksolar',
    'aurora',
  }
  local color = options[math.random(#options)]
  print('Random color scheme: ' .. color)
  return color
end

M.randomColorScheme = function()

  local scheme = M.randomSchemeName()
  M.applyColorScheme(scheme)
end

return M
