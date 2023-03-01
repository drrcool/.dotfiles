local keymap = vim.keymap.set

-- Better escape with jk
keymap("i", "jk", "<ESC>")
keymap("t", "jk", "<C-\\><C-n>")
keymap("t", "<C-h>", "<C-\\><C-n><C-w>h")
keymap("t", "<C-j>", "<C-\\><C-n><C-w>j")
keymap("t", "<C-k>", "<C-\\><C-n><C-w>k")
keymap("t", "<C-l>", "<C-\\><C-n><C-w>l")

-- Better indent
keymap("v", "<", "<gv")
keymap("v", ">", ">gv")

-- Paste over selected text without yanking it
keymap("v", "p", '"_dP')

-- Move Lines
keymap("n", "<A-j>", ":m .+1<CR>==")
keymap("v", "<A-j>", ":m '>+1<CR>gv=gv")
keymap("i", "<A-j>", "<Esc>:m .+1<CR>==gi")
keymap("n", "<A-k>", ":m .-2<CR>==")
keymap("v", "<A-k>", ":m '<-2<CR>gv=gv")
keymap("i", "<A-k>", "<Esc>:m .-2<CR>==gi")

-- Resize Windows with Shift-Arrows
keymap("n", "<S-Up>", "<cmd>resize +2<CR>")
keymap("n", "<S-Down>", "<cmd>resize -2<CR>")
keymap("n", "<S-Left>", "<cmd>vertical resize -2 <CR>")
keymap("n", "<S-Right>", "<cmd>vertical resize +2<CR>")
