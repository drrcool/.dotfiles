local keymap = vim.keymap.set

-- Better viewing 
keymap("n","n", "nzzzv")
keymap("n","N", "Nzzzv")
keymap("n", "g", "g,zvzz")
keymap("n", "G", "G;zvzz")

-- Scrolling
keymap("n", "<C-d>", "<C-d>zz")
keymap("n", "<C-u>", "<C-u>zz")

-- Better excape using jk in insert and terminal mode
keymap("i", "jk", "<ESC>")
keymap("t", "jk", "<C-\\><C-n>")
keymap("t", "<C-h>", "<C-\\><C-n><C-w>h")
keymap("t", "<C-j>", "<C-\\><C-n><C-w>j")
keymap("t", "<C-k>", "<C-\\><C-n><C-w>k")
keymap("t", "<C-l>", "<C-\\><C-n><C-w>l")

-- Better indent
keymap("v", "<", "<gv")
keymap("v", ">", ">gv")

-- Paste over currently selected text without yanking
keymap("v", "p", "_dp")


