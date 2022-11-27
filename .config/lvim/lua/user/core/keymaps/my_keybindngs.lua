lvim.leader = "space"
lvim.keys.normal_mode['<C-[>'] = ":bprev<CR>"
lvim.keys.normal_mode['<C-]>'] = ":bnext<CR>"

-- Splits
lvim.keys.normal_mode['<leader>ss'] = '<C-w>ss'
lvim.keys.normal_mode['<leader>sv'] = '<C-w>v'
lvim.keys.normal_mode['<leader>sh'] = '<C-w>h'
lvim.keys.normal_mode['<leader>sj'] = '<C-w>j'
lvim.keys.normal_mode['<leader>sk'] = '<C-w>k'
lvim.keys.normal_mode['<leader>sl'] = '<C-w>l'
lvim.keys.normal_mode['<A-h>'] = ":bprev<CR>"
lvim.keys.normal_mode['<A-l>'] = ":bnext<CR>"
lvim.keys.normal_mode['<leader>sq'] = '<C-w>q'
lvim.keys.normal_mode['<leader>sx '] = ':close<CR>'

lvim.keys.normal_mode['<leader>sm '] = ':MaximizerToggle<CR>'
lvim.keys.normal_mode['<leader>ll '] = ':LspRestart<CR>'


lvim.keys.visual_mode['<'] = '<gv'
lvim.keys.visual_mode['<'] = '>gv'
lvim.keys.visual_mode['p'] = '_dP'

lvim.keys.visual_mode['<leader>/'] = ':Commentary<CR>'
lvim.keys.normal_mode['<leader>/'] = ':Commentary<CR>'
