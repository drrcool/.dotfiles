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

M.randomColorScheme = function()

  local options = { 'nightfox', 'tokyonight', 'catppuccin',
    'bubblegum', 'prmths', 'burnttoast256', 'SerialExperimentsLain', 'stereokai', 'wellsokai',
    'cobalt', 'deep-space', 'seti', 'skittles_berry', 'gryffin', 'jellybeans', 'heroku-terminal',
    'jellybeans', 'lanox', 'monokai', 'liquidcarbon', 'smarties', 'spacemacs-theme', 'materialtheme',
    'minimalist', 'molokai', 'monokai-chris', 'monokain', 'mrkn256', 'nightflight2', 'PerfectDark',
    'srcery', 'moonfly', 'murphy', 'trivial256' }

  local color = options[math.random(#options)]
  print('Random color scheme: ' .. color)
  M.applyColorScheme(color)
end

return M
