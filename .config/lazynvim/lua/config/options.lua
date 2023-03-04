-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here
vim.g.maplocalleader = ","

local indent = 2
vim.o.formatoptions = "jcroqlnt"
vim.o.shortmess = "filnxtToOfWIcC"

local o = vim.opt
o.breakindent = true
o.cmdheight = 0
o.completeopt = "menuone,noselect"
o.conceallevel = 3
o.confirm = true
o.cursorline = true
o.expandtab = true
o.hidden = true
o.hlsearch = true
o.ignorecase = true
o.inccommand = "nosplit"
o.joinspaces = false
o.laststatus = 0
o.list = true
o.mouse = "a"
o.number = true
o.pumblend = 10
o.pumheight = 10
o.relativenumber = true
o.scrolloff = 8
o.sessionoptions = { "buffers", "curdir", "tabpages", "winsize" }
o.shiftround = true
o.shiftwidth = indent
o.showmode = false
o.sidescrolloff = 8
o.signcolumn = "yes"
o.smartcase = true
o.smartindent = true
o.splitbelow = true
o.splitkeep = "screen"
o.splitright = true
o.tabstop = indent
o.termguicolors = true
o.timeoutlen = 300
o.undofile = true
o.updatetime = 200
o.wildmode = "longest:full,full"
o.laststatus = 3

vim.g.mapleader = " "
vim.g.maplocalleader = ","
vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })