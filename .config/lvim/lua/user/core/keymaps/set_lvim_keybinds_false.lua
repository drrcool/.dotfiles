-- Here we  set all lvim keybindings to false, so the only ones we can use are those we set.

lvim.keys.insert_mode = {
  ["jj"] = false,

  -- 'jk' for quitting insert mode
  ["jk"] = "<ESC>",
  -- 'kj' for quitting insert mode
  ["kj"] = false,
  -- 'jj' for quitting insert mode
  -- Move current line / block with Alt-j/k ala vscode.
  ["<A-j>"] = "<Esc>:m +1<CR>",
  -- Move current line / block with Alt-j/k ala vscode.
  ["<A-k>"] = "<Esc>:m -2<CR>",
  -- navigation
  ["<A-Up>"] = false,
  ["<A-Down>"] = false,
  ["<A-Left>"] = false,
  ["<A-Right>"] = false,
}

lvim.keys.normal_mode = {
  -- Better window movement
  ["<C-h>"] = "<C-w>h",
  ["<C-j>"] = "<C-w>j",
  ["<C-k>"] = "<C-w>k",
  ["<C-l>"] = "<C-w>l",

  -- Resize with arrows
  ["<C-Up>"] = ":resize -2<CR>",
  ["<C-Down>"] = ":resize +2<CR>",
  ["<C-Left>"] = ":vertical resize -2<CR>",
  ["<C-Right>"] = ":vertical resize +2<CR>",

  -- Tab switch buffer
  ["<S-l>"] = false,
  ["<S-h>"] = false,

  -- Move current line / block with Alt-j/k a la vscode.
  ["<A-j>"] = "<Esc>:m  +1<CR>",
  ["<A-k>"] = "<Esc>:m -2<CR>",

  -- QuickFix
  ["]q"] = ":cnext<CR>",
  ["[q"] = ":cprevious<CR>",
  ["<C-q>"] = ":copen<CR>"
}
