local opts = { noremap = true, silent = true }

local term_opts = { silent = true }

-- Shorten function name
local keymap = vim.api.nvim_set_keymap

--Remap space as leader key
keymap("", "<Esc>", "<Nop>", opts)
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.keymap.set("n", "gs", "^")
vim.keymap.set("n", "gl", "$")
vim.keymap.set("n", "<leader>ss", ":split<Return><C-w>w")
vim.keymap.set("n", "<leader>sv", ":vsplit<Return><C-w>w")
vim.keymap.set("n", "<leader>sh", "<C-w>h")
vim.keymap.set("n", "<C-n>", ":bnext<CR>")
vim.keymap.set("n", "<C-p>", ":bprev<CR>")
vim.keymap.set("n", "<C-t>", "<C-^>")
vim.keymap.set("n", "<C-x>", ":bp <bar> bd#<CR>")


keymap("n", "<C-]>", ":bnext<CR>", opts)
keymap("n", "<C-[>", ":bprevious<CR>", opts)


-- Insert --
-- Press jk fast to: exit insert mode
keymap("i", "jk", "<ESC>", opts)

-- Visual --
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Move text up and downj
keymap("v", "p", '"_dP', opts)

-- Visual Block --
-- Move text up and down
keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
keymap("x", "K", ":move '<-2<CR>gv-gv", opts)
keymap("x", "<A-j>", ":move '>+1<CR>gv-gv", opts)
keymap("x", "<A-k>", ":move '<-2<CR>gv-gv", opts)

-- Comments
keymap('n', '<leader>/', ':Commentary<CR>', opts)
keymap('v', '<leader>/', ':Commentary<CR>', opts)
keymap('x', '<leader>/', ':Commentary<CR>', opts)

-- Terminal --
-- Better terminal navigation
keymap("t", "<leader>sh", "<C-\\><C-N><C-w>h", term_opts)
keymap("t", "<leader>sj", "<C-\\><C-N><C-w>j", term_opts)
keymap("t", "<leader>sk", "<C-\\><C-N><C-w>k", term_opts)
keymap("t", "<leader>sl", "<C-\\><C-N><C-w>l", term_opts)
