-- Setup Leader Key
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- set hightlight on search
vim.o.hlsearch = true
vim.opt.incsearch = true

-- Make line numbers default
vim.opt.relativenumber = true
vim.opt.nu = true

-- Enable Mouse Mode
vim.o.mouse = "a"

-- Sync clipboard between OS and Neovim
vim.o.clipboard = "unnamedplus"

-- Enable break indent
vim.o.breakindent = true

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.wo.signcolumn = "yes"

-- Decrease update time
vim.o.updatetime = 50
vim.o.timeout = true
vim.o.timeoutlen = 300

-- Set completeopt to have better completion experience
vim.o.completeopt = "menuone,noselect"

--Turn on Colors
vim.o.termguicolors = true

-- Setup tabs
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

-- Disable backups and swaps
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undofile = true



vim.keymap.set({"n","v"}, "<Space>", "<Nop>", {silent=true})
