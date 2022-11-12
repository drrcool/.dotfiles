pcall(require, 'impatient')

--  This is a good place to setup  globals we think should always be ready\
vim.g.mapleader = " "
vim.g.maplocalleader = ","
require 'rcool.globals'


--  LSP
require 'rcool.lsp'

-- Telescope 
require 'rcool.telescope.setup'`
require 'rcool.telescope.mappings'

