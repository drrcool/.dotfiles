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

