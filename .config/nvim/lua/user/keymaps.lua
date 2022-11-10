local opts = { noremap = true, silent = true }

local term_opts = { silent = true }

-- Shorten function name
local keymap = vim.api.nvim_set_keymap

--Remap space as leader key
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "


-- SPlits
vim.keymap.set("n", "<leader>ss", ":split<Return><C-w>w")
vim.keymap.set("n", "<leader>sv", ":vsplit<Return><C-w>w")
vim.keymap.set("n", "<leader>sh", "<C-w>h")
vim.keymap.set("n", "<leader>sj", "<C-w>j")
vim.keymap.set("n", "<leader>sk", "<C-w>k")
vim.keymap.set("n", "<leader>sl", "<C-w>l")
vim.keymap.set("n", "<leader>sq", "<C-w>q")


-- Resize with arrows
keymap("n", "<C-Up>", ":resize -2<CR>", opts)
keymap("n", "<C-Down>", ":resize +2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize -2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize +2<CR>", opts)

vim.keymap.set("n", "<C-n>", ":BufSurfForward<CR>", { silent = true, noremap = true })
vim.keymap.set("n", "<C-p>", ":BufSurfBack<CR>", { silent = true, noremap = true })
vim.keymap.set("n", "<C-t>", "<C-^>")
vim.keymap.set("n", "<C-x>", ":bp <bar> bd#<CR>")


keymap("n", "<C-l>", ":bnext<CR>", opts)
keymap("n", "<C-h>", ":bprevious<CR>", opts)

-- Move text up and down
keymap("n", "<A-j>", "<Esc>:m .+1<CR>==gi", opts)
keymap("n", "<A-k>", "<Esc>:m .-2<CR>==gi", opts)

-- Insert --
-- Press jk fast to exit insert mode
keymap("i", "jk", "<ESC>", opts)

-- Visual --
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Move text up and downj
keymap("v", "<A-j>", ":m .+1<CR>==", opts)
keymap("v", "<A-k>", ":m .-2<CR>==", opts)
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
