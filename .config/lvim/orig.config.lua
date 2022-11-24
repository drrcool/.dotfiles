--[[ 
lvim is the global options object

Linters should be
filled in as strings with either
a global executable or a path to
an executable
]]
-- THESE ARE EXAMPLE CONFIGS FEEL FREE TO CHANGE TO WHATEVER YOU WANT
local colorscheme = require 'user.colorscheme'
-- general
lvim.log.level = "warn"
lvim.format_on_save = true
lvim.colorscheme = colorscheme.randomSchemeName()
lvim.transparent_window = true
vim.opt.relativenumber = true
-- to disable icons and use a minimalist setup, uncomment the following
-- lvim.use_icons = false

-- keymappings [view all the defaults by pressing <leader>Lk]
lvim.leader                         = "space"
-- add your own keymapping
lvim.keys.normal_mode["<C-s>"]      = ":w<cr>"
lvim.keys.normal_mode["<S-l>"]      = ":BufferLineCycleNext<CR>"
lvim.keys.normal_mode["<S-h>"]      = ":BufferLineCyclePrev<CR>"
lvim.keys.normal_mode["<leader>ss"] = "<C-w>ss"
lvim.keys.normal_mode["<leader>sv"] = "<C-w>v"
lvim.keys.normal_mode["<C-Up>"]     = ":resize -2<CR>"
lvim.keys.normal_mode["<C-Down>"]   = ":resize +2<CR>"
lvim.keys.normal_mode["<C-Left>"]   = ":vertical resize -2<CR>"
lvim.keys.normal_mode["<C-Right>"]  = ":vertical resize -2<CR>"
lvim.keys.normal_mode["<A-j>"]      = "<Esc>:m .+1<CR>"
lvim.keys.normal_mode["<A-k>"]      = "<Esc>:m .-2<CR>"
lvim.keys.normal_mode["<leader>ss"] = ":split<Return><C-w>w"
lvim.keys.normal_mode["<leader>sv"] = ":vsplit<Return><C-w>w"
lvim.keys.normal_mode["<leader>sh"] = "<C-w>h"
lvim.keys.normal_mode["<leader>sj"] = "<C-w>j"
lvim.keys.normal_mode["<leader>sk"] = "<C-w>k"
lvim.keys.normal_mode["<leader>sl"] = "<C-w>l"
lvim.keys.normal_mode["<leader>sq"] = "<C-w>q"
lvim.keys.normal_mode["<C-h>"]      = "<C-w>h"
lvim.keys.normal_mode["<C-j>"]      = "<C-w>j"
lvim.keys.normal_mode["<C-k>"]      = "<C-w>k"
lvim.keys.normal_mode["<C-l>"]      = "<C-w>l"
lvim.keys.normal_mode['<M-h>']      = ":bprevious<CR>"
lvim.keys.normal_mode['<M-l>']      = ":bnext<CR>"

-- Insert --
-- Press jk fast to exit insert mode
lvim.keys.insert_mode["jk"] = "<ESC>"

-- Visual --
-- Stay in indent mode
lvim.keys.visual_mode["<"] = "<gv"
lvim.keys.visual_mode[">"] = ">gv"
lvim.keys.visual_mode["p"] = "_dP"
lvim.keys.visual_mode["<A-j>"] = "<Esc>:m .+1<CR>"
lvim.keys.visual_mode["<A-k>"] = "<Esc>:m .-2<CR>"

-- Visual Block --
-- Move text up and down
lvim.keys.visual_mode["J"] = ":move '>+1<CR>gv-gv"
lvim.keys.visual_mode["K"] = ":move '<-2<CR>gv-gv"

-- }

lvim.keys.insert_mode["<leader>/"] = "<cmd>Commentary<CR>"
lvim.keys.normal_mode["<leader>/"] = "<cmd>Commentary<CR>"
lvim.keys.visual_mode["<leader>/"] = "<cmd>Commentary<CR>"

local term_opts = { silent = true }
local keymap = vim.api.nvim_set_keymap
keymap("t", "<leader>sh", "<C-\\><C-N><C-w>h", term_opts)
keymap("t", "<leader>sj", "<C-\\><C-N><C-w>j", term_opts)
keymap("t", "<leader>sk", "<C-\\><C-N><C-w>k", term_opts)
keymap("t", "<leader>sl", "<C-\\><C-N><C-w>l", term_opts)




lvim.builtin.which_key.mappings = {
  ["L"] = { "<cmd>Legendary<cr>", "Legendary" },
  ["C"] = { "<cmd>lua require('telescope').extensions.neoclip.default()<cr>", "Neoclip" },
  ["a"] = { "<cmd>Alpha<cr>", "Alpha" },
  ["e"] = { "<cmd>Neotree float reveal_force_cwd<cr>", "Explorer" },
  ["w"] = { "<cmd>w!<CR>", "Save" },
  ["q"] = { "<cmd>q!<CR>", "Quit" },
  ["h"] = { "<cmd>nohlsearch<CR>", "No Highlight" },
  ["P"] = { "<cmd>lua require('telescope').load_extension('projects').projects()<cr>", "Projects" },
  ["R"] = { "<cmd>source $MYVIMRC<cr>", "Reload" },
  ['cc'] = { "<cmd>lua require('user.colorscheme').randomColorScheme()<CR>", "Random Colo" },
  ["?"] = { "<cmd>Cheatsheet<cr>", "Cheatsheet" },

  f = {
    name = "Find",
    a = { '<cmd>lua require(\'telescope.builtin\').commands()<cr>', 'Commands' },
    b = { '<cmd>lua require(\'telescope.builtin\').buffers()<cr>', 'Buffers' },
    c = { "<cmd>lua require('telescope.builtin').colorscheme()<cr>", "Colorscheme" },
    e = { '<cmd>NvimTreeToggle<cr>', 'Explorer' },
    f = { '<cmd>lua require(\'telescope.builtin\').find_files()<cr>', 'Files' },
    g = {
      '<cmd>lua require(\'telescope.builtin\').live_grep()<cr>',
      'Live Grep',
    },
    h = { "<cmd>lua require('telescope.builtin').help_tags()<cr>", "Find Help" },
    k = { "<cmd>lua require('telescope.builtin').keymaps()<cr>", "Keymaps" },
    m = { '<cmd>lua require(\'telescope.builtin\').marks()<cr>', 'Marks' },
    n = { '<cmd>Telescope notify<CR>', "Notifications" },
    o = {
      '<cmd>lua require(\'telescope.builtin\').oldfiles()<cr>',
      'Old Files',
    },
    w = {
      '<cmd>lua require(\'telescope.builtin\').current_buffer_fuzzy_find()<cr>',
      'Current Buffer',
    },
    C = { "<cmd> lua require('telescope').extensions.neoclip.default()<cr>", "Clipboard" },
    B = { "<cmd> lua require('telescope.builtin').git_branches()<cr>", "Git Branches" },
    M = { "<cmd>lua require('telescope.builtin').man_pages()<cr>", "Man Pages" },
    R = { "<cmd>lua require('telescope.builtin').registers()<cr>", "Registers" },
  },
  p = {
    name = "Packer",
    c = { "<cmd>PackerCompile<cr>", "Compile" },
    i = { "<cmd>PackerInstall<cr>", "Install" },
    s = { "<cmd>PackerSync<cr>", "Sync" },
    S = { "<cmd>PackerStatus<cr>", "Status" },
    u = { "<cmd>PackerUpdate<cr>", "Update" },
  },
  g = {
    name = 'Goto',
    d = { '<Cmd>lua vim.lsp.buf.definition()<CR>', 'Definition' },
    -- d = { "<cmd>lua require('goto-preview').goto_preview_definition()<CR>", "Definition" },
    D = { '<Cmd>lua vim.lsp.buf.declaration()<CR>', 'Declaration' },
    h = { '<cmd>lua vim.lsp.buf.signature_help()<CR>', 'Signature Help' },
    I = { '<cmd>Telescope lsp_implementations<CR>', 'Goto Implementation' },
    b = { '<cmd>lua vim.lsp.buf.type_definition()<CR>', 'Goto Type Definition' },
    l = { '<cmd>lua vim.diagnostic.open_float()<CR>', 'Hover Diagnostic' },
    n = { '<cmd>vim.diagnostic.goto_next()<CR>', 'Next Diagnostic' },
    p = { '<cmd>vim.diagnostic.goto_prev()<CR>', 'Prev Diagnostic' },
    -- b = { "<cmd>lua require('goto-preview').goto_preview_type_definition()<CR>", "Goto Type Definition" },
  },
  G = {
    name = "Git",
    g = { "<cmd>lua _LAZYGIT_TOGGLE()<CR>", "Lazygit" },
    j = { "<cmd>lua require('gitsigns').next_hunk()<cr>", "Next Hunk" },
    k = { "<cmd>lua require('gitsigns').prev_hunk()<cr>", "Prev Hunk" },
    l = { "<cmd>lua require('gitsigns').blame_line()<cr>", "Blame" },
    p = { "<cmd>lua require('gitsigns').preview_hunk()<cr>", "Preview Hunk" },
    r = { "<cmd>lua require('gitsigns').reset_hunk()<cr>", "Reset Hunk" },
    R = { "<cmd>lua require('gitsigns').reset_buffer()<cr>", "Reset Buffer" },
    s = { "<cmd>lua require('gitsigns').stage_hunk()<cr>", "Stage Hunk" },
    u = { "<cmd>lua require('gitsigns').undo_stage_hunk()<cr>", "Undo Stage Hunk" },
    o = { "<cmd>lua require('telescope.builtin').git_status()<cr>", "Open changed file" },
    b = { "<cmd>lua require('telescope.builtin').git_branches()<cr>", "Checkout branch" },
    c = { "<cmd>lua require('telescope.builtin').git_commits()<cr>", "Checkout commit" },
    d = {
      "<cmd>Gitsigns diffthis HEAD<cr>",
      "Diff",
    },
    D = {
      name = "Diffview",
      o = { "<cmd>DiffviewOpen<CR>", "Diffview Open" },
      c = { "<cmd>DiffviewClose<CR>", "Diffview Close" }
    }
  },

  s = {
    name = "Search",
    h = { '<C-w>h', "Move Left" },
    j = { '<C-w>j', "Move Down" },
    k = { '<C-w>k', "Move Up" },
    l = { '<C-w>l', "Move right" },
  },

  t = {
    name = "Terminal",
    n = { "<cmd>lua _NODE_TOGGLE()<cr>", "Node" },
    u = { "<cmd>lua _NCDU_TOGGLE()<cr>", "NCDU" },
    t = { "<cmd>lua _HTOP_TOGGLE()<cr>", "Htop" },
    p = { "<cmd>lua _PYTHON_TOGGLE()<cr>", "Python" },
    f = { "<cmd>ToggleTerm direction=float<cr>", "Float" },
    h = { "<cmd>ToggleTerm size=10 direction=horizontal<cr>", "Horizontal" },
    v = { "<cmd>ToggleTerm size=80 direction=vertical<cr>", "Vertical" },
  },


  l = {
    name = "LSP",
    D = { "<cmd>lua vim.lsp.buf.declaration()<cr>", "Goto Declaration" },
    H = { "<cmd>lua vim.lsp.buf.signature_help()<cr>", "Signature Help" },
    R = { "<cmd>lua require('telescope.builtin').lsp_references()<cr>", "References" },
    S = { "<cmd>lua require('telescope.builtin').lsp_workspace_symbols()<cr>", "Workspace Symbols" },
    a = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "Code Action" },
    d = { "<cmd>lua require('telescope.builtin').lsp_definitions()<cr>", "Goto Definition" },
    f = { "<cmd>lua vim.lsp.buf.format({async = true})<cr>", "Format" },
    h = { "<cmd>lua vim.lsp.buf.hover()<cr>", "Hover" },
    i = { "<cmd>lua require('telescope.builtin').lsp_implementations()<cr>", "Implimentations" },
    j = { "<cmd>lua vim.diagnostic.goto_next()<cr>", "Next Diagnostic" },
    k = { "<cmd>lua vim.diagnostic.goto_prev()<cr>", "Prev Diagnostic" },
    l = { "<cmd>lua vim.lsp.codelens.run()<cr>", "CodeLens Action" },
    q = { "<cmd>lua require('telescope.builtin').quickfix()<cr>", "Quickfix" },
    r = { "<cmd>lua vim.lsp.buf.rename()<cr>", "Rename" },
    s = { "<cmd>lua require('telescope.builtin').lsp_document_symbols()<cr>", "Document Symbols" },
    t = { "<cmd>lua require('telescope.builtin').lsp_type_definitions()<cr>", "Type Definition" },
    w = { "<cmd>Telescope lsp_workspace_diagnostics<CR>", "Workspace Diagnostics" },
    o = { "<cmd>SymbolsOutline<cr>", "Outline" },
    O = { "<cmd>TodoTelescope<cr>", "Todo" },
    P = {
      name = "Peek",
      d = { "<cmd>lua require('user.lsp.peek').Peek('definition')<cr>", "Definition" },
      t = { "<cmd>lua require('user.lsp.peek').Peek('typeDefinition')<cr>", "Type Definition" },
      i = { "<cmd>lua require('user.lsp.peek').Peek('implementation')<cr>", "Implementation" },
    },
    T = {
      name = "Trouble",
      t = { "<cmd>TroubleToggle<cr>", "Toggle" },
      w = { "<cmd>TroubleToggle workspace_diagnostics<cr>", "Workspace Diagnostics" },
      d = { "<cmd>TroubleToggle document_diagnostics<cr>", "Document Diagnostics" },
      q = { "<cmd>TroubleToggle quickfix<cr>", "QuickFix" },
      l = { "<cmd>TroubleToggle loclist<cr>", "Loc List" },
      L = { "<cmd>TroubleToggle lsp_references<cr>", "Reference" },
    },
  },

  b = {
    name = "Buffers",
    j = { "<cmd>BufferLinePick<cr>", "Jump" },
    f = { require("telescope.builtin").buffers, "Find" },
    b = { "<cmd>BufferLineCyclePrev<cr>", "Previous" },
    n = { "<cmd>BufferLineCycleNext<cr>", "Next" },
    e = {
      "<cmd>BufferLinePickClose<cr>",
      "Pick which buffer to close",
    },
    h = { "<cmd>BufferLineCloseLeft<cr>", "Close all to the left" },
    l = {
      "<cmd>BufferLineCloseRight<cr>",
      "Close all to the right",
    },
    D = {
      "<cmd>BufferLineSortByDirectory<cr>",
      "Sort by directory",
    },
    L = {
      "<cmd>BufferLineSortByExtension<cr>",
      "Sort by language",
    },
  },

  m = {
    name = "Motion",
    h = { "<cmd>HopWord<cr>", "Hop" },
    t = { "<cmd>tabnew<cr>", "New Tab" },
    n = { "<cmd>tabn<cr>", "Next Tab" },
    p = { "<cmd>tabp<cr>", "Previous Tab" },
    c = { "<cmd>tabclose<cr>", "Close Tab" },
  },

  n = {
    name = "Neotest",
    a = { "<cmd>lua require('neotest').run.attach()<cr>", "Attach" },
    f = { "<cmd>lua require('neotest').run.run(vim.fn.expand('%'))<cr>", "Run File" },
    F = { "<cmd>lua require('neotest').run.run({vim.fn.expand('%'), strategy = 'dap'})<cr>", "Debug File" },
    l = { "<cmd>lua require('neotest').run.run_last()<cr>", "Run Last" },
    L = { "<cmd>lua require('neotest').run.run_last({ strategy = 'dap' })<cr>", "Debug Last" },
    n = { "<cmd>lua require('neotest').run.run()<cr>", "Run Nearest" },
    N = { "<cmd>lua require('neotest').run.run({strategy = 'dap'})<cr>", "Debug Nearest" },
    o = { "<cmd>lua require('neotest').output.open({ enter = true })<cr>", "Output" },
    S = { "<cmd>lua require('neotest').run.stop()<cr>", "Stop" },
    s = { "<cmd>lua require('neotest').summary.toggle()<cr>", "Summary" },
    v = { "<cmd>TestVisit<cr>", "Visit" },
    x = { "<cmd>TestSuite<cr>", "Suite" },
  },
  o = {
    name = "Overseer",
    C = { "<cmd>OverseerClose<cr>", "OverseerClose" },
    a = { "<cmd>OverseerTaskAction<cr>", "OverseerTaskAction" },
    b = { "<cmd>OverseerBuild<cr>", "OverseerBuild" },
    c = { "<cmd>OverseerRunCmd<cr>", "OverseerRunCmd" },
    d = { "<cmd>OverseerDeleteBundle<cr>", "OverseerDeleteBundle" },
    l = { "<cmd>OverseerLoadBundle<cr>", "OverseerLoadBundle" },
    o = { "<cmd>OverseerOpen!<cr>", "OverseerOpen" },
    q = { "<cmd>OverseerQuickAction<cr>", "OverseerQuickAction" },
    r = { "<cmd>OverseerRun<cr>", "OverseerRun" },
    s = { "<cmd>OverseerSaveBundle<cr>", "OverseerSaveBundle" },
    t = { "<cmd>OverseerToggle!<cr>", "OverseerToggle" },
  },
  S = {
    name = "Session",
    c = { "<cmd>lua require('persistence').load()<cr>", "Restore last session for current dir" },
    l = { "<cmd>lua require('persistence').load({ last = true })<cr>", "Restore last session" },
    Q = { "<cmd>lua require('persistence').stop()<cr>", "Quit without saving session" },
  },

  d = {
    name = "Debug",
    b = { "<cmd>lua require'dap'.toggle_breakpoint()<cr>", "Breakpoint" },
    c = { "<cmd>lua require'dap'.continue()<cr>", "Continue" },
    i = { "<cmd>lua require'dap'.step_into()<cr>", "Into" },
    o = { "<cmd>lua require'dap'.step_over()<cr>", "Over" },
    O = { "<cmd>lua require'dap'.step_out()<cr>", "Out" },
    r = { "<cmd>lua require'dap'.repl.toggle()<cr>", "Repl" },
    l = { "<cmd>lua require'dap'.run_last()<cr>", "Last" },
    u = { "<cmd>lua require'dapui'.toggle()<cr>", "UI" },
    x = { "<cmd>lua require'dap'.terminate()<cr>", "Exit" },
  },
  v = {
    name = "Versions (PackageJSON)",
    s = { "<cmd>lua require'package-info'.show<CR>", "Toggle Package Versions" },
    u = { "<cmd>lua require'package-info'.update()<CR>", "Update This Package" },
    d = { "<cmd>lua require'package-info'.delete()<CR>", "Delete This Package" },
    i = { "<cmd>lua require'package-info'.install()<CR>", "Install new dependency" },
    v = { "<cmd>lua require'package-info'.change_version<CR>", "Install a different version" }

  },
  z = {
    name = "Zen",
    a = { "<cmd>TZAtaraxis<cr>", "Ataraxis" },
    m = { "<cmd>TZMinimalist<cr>", "Minimalist" },
    n = { "<cmd>TZNarrow<cr>", "Narrow" },
    f = { "<cmd>TZFocus<cr>", "Focus" },
  },
}

-- TODO: User Config for predefined plugins
-- After changing plugin config exit and reopen LunarVim, Run :PackerInstall :PackerCompile
lvim.builtin.alpha.active = true
lvim.builtin.alpha.mode = "dashboard"
lvim.builtin.terminal.active = true
lvim.builtin.nvimtree.setup.view.side = "left"
lvim.builtin.nvimtree.setup.renderer.icons.show.git = false

-- if you don't want all the parsers change this to a table of the ones you want
lvim.builtin.treesitter.ensure_installed = {
  "bash",
  "c",
  "javascript",
  "json",
  "lua",
  "python",
  "typescript",
  "tsx",
  "css",
  "rust",
  "java",
  "yaml",
}

lvim.builtin.treesitter.ignore_install = { "haskell" }
lvim.builtin.treesitter.highlight.enable = true
lvim.builtin.treesitter.rainbow.enable = true

-- generic LSP settings

-- -- make sure server will always be installed even if the server is in skipped_servers list
-- lvim.lsp.installer.setup.ensure_installed = {
--     "sumeko_lua",
--     "jsonls",
-- }
-- change UI setting of `LspInstallInfo`
-- see <https://github.com/williamboman/nvim-lsp-installer#default-configuration>
-- lvim.lsp.installer.setup.ui.check_outdated_servers_on_open = false
-- lvim.lsp.installer.setup.ui.border = "rounded"
-- lvim.lsp.installer.setup.ui.keymaps = {
-- uninstall_server = "d",
-- toggle_server_expand = "o",
-- }

-- ---@usage disable automatic installation of servers
-- lvim.lsp.installer.setup.automatic_installation = false

-- ---configure a server manually. !!Requires `:LvimCacheReset` to take effect!!
-- ---see the full default list `:lua print(vim.inspect(lvim.lsp.automatic_configuration.skipped_servers))`
-- vim.list_extend(lvim.lsp.automatic_configuration.skipped_servers, { "pyright" })
-- local opts = {} -- check the lspconfig documentation for a list of all possible options
-- require("lvim.lsp.manager").setup("pyright", opts)

-- ---remove a server from the skipped list, e.g. eslint, or emmet_ls. !!Requires `:LvimCacheReset` to take effect!!
-- ---`:LvimInfo` lists which server(s) are skipped for the current filetype
-- lvim.lsp.automatic_configuration.skipped_servers = vim.tbl_filter(function(server)
--   return server ~= "emmet_ls"
-- end, lvim.lsp.automatic_configuration.skipped_servers)

-- -- you can set a custom on_attach function that will be used for all the language servers
-- -- See <https://github.com/neovim/nvim-lspconfig#keybindings-and-completion>
-- lvim.lsp.on_attach_callback = function(client, bufnr)
--   local function buf_set_option(...)
--     vim.api.nvim_buf_set_option(bufnr, ...)
--   end
--   --Enable completion triggered by <c-x><c-o>
--   buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")
-- end

-- -- set a formatter, this will override the language server formatting capabilities (if it exists)
local formatters = require "lvim.lsp.null-ls.formatters"
formatters.setup {
  { command = "black", filetypes = { "python" } },
  {
    -- each formatter accepts a list of options identical to https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md#Configuration
    command = "prettier",
    ---@usage arguments to pass to the formatter
    -- these cannot contain whitespaces, options such as `--line-width 80` become either `{'--line-width', '80'}` or `{'--line-width=80'}`
    extra_args = { "--print-with", "100" },
    ---@usage specify which filetypes to enable. By default a providers will attach to all the filetypes it supports.
    filetypes = { "javascript", "javascriptreact", "typescript", "typescriptreact" },
  },
}

-- set additional linters
local linters = require "lvim.lsp.null-ls.linters"
linters.setup {
  { command = "flake8", filetypes = { "python" } },
  {
    -- each linter accepts a list of options identical to https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md#Configuration
    command = "shellcheck",
    ---@usage arguments to pass to the formatter
    -- these cannot contain whitespaces, options such as `--line-width 80` become either `{'--line-width', '80'}` or `{'--line-width=80'}`
    extra_args = { "--severity", "warning" },
  },
  {
    command = "codespell",
    ---@usage specify which filetypes to enable. By default a providers will attach to all the filetypes it supports.
    filetypes = { "javascript", "python", "typescript", "javascriptreact", "typescriptreact" },
  },
}

-- Additional Plugins
lvim.plugins = {
  {
    "folke/trouble.nvim",
    cmd = "TroubleToggle",
  },

  {
    "nvim-neotest/neotest",
    requires = {
      "nvim-lua/plenary.nvim",
      "vim-test/vim-test",
      "nvim-treesitter/nvim-treesitter",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-neotest/neotest-python",
      "haydenmeade/neotest-jest",
    },
    config = function()
      require('user.neotest')
    end
  },
  -- Task Runner
  { 'stevearc/overseer.nvim',
    config = function()
      require('overseer').setup()
    end
  },
  { 'EdenEast/nightfox.nvim',
    config = function()
      require('user.nightfox').setup()
    end
  }, { 'catppuccin/nvim', as = 'catppuccin' },
  {
    'flazz/vim-colorschemes'
  },
  { "bignimbus/pop-punk.vim" },
  { "krfl/fleetish-vim" },
  { "tomasr/molokai" },
  { "mrjones2014/legendary.nvim" },
  { 'ggandor/lightspeed.nvim' },
  {
    "wakatime/vim-wakatime",
  },
  {
    "tpope/vim-surround",
    keys = { "c", "d", "y" },
    config = function()
      vim.cmd("nmap ds       <Plug>Dsurround")
      vim.cmd("nmap cs       <Plug>Csurround")
      vim.cmd("nmap cS       <Plug>CSurround")
      vim.cmd("nmap ys       <Plug>Ysurround")
      vim.cmd("nmap yS       <Plug>YSurround")
      vim.cmd("nmap yss      <Plug>Yssurround")
      vim.cmd("nmap ySs      <Plug>YSsurround")
      vim.cmd("nmap ySS      <Plug>YSsurround")
      vim.cmd("xmap gs       <Plug>VSurround")
      vim.cmd("xmap gS       <Plug>VgSurround")
      vim.o.timeoutlen = 1000
    end,
  },
  {
    "kevinhwang91/nvim-bqf",
    event = { "BufRead", "BufNew" },
    config = function()
      require("bqf").setup({
        auto_enable = true,
        preview = {
          win_height = 12,
          win_vheight = 12,
          delay_syntax = 80,
          border_chars = { "┃", "┃", "━", "━", "┏", "┓", "┗", "┛", "█" },
        },
        func_map = {
          vsplit = "",
          ptogglemode = "z,",
          stoggleup = "",
        },
        filter = {
          fzf = {
            action_for = { ["ctrl-s"] = "split" },
            extra_opts = { "--bind", "ctrl-o:toggle-all", "--prompt", "> " },
          },
        },
      })
    end,
  },

  {
    "windwp/nvim-ts-autotag",
    config = function()
      require("nvim-ts-autotag").setup()
    end,
  },
  {
    "p00f/nvim-ts-rainbow",
  },

  { "nvim-telescope/telescope-file-browser.nvim" },
  { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },

  { "github/copilot.vim" },
  { 'rcarriga/nvim-notify' },
  {
    "zbirenbaum/copilot-cmp",
    after = { "copilot.lua" },
    config = function()
      require("copilot_cmp").setup()
    end
  },
  {
    "zbirenbaum/copilot.lua",
    event = "VimEnter",
    config = function()
      vim.defer_fn(function()
        require("copilot").setup({
          panel = {
            auto_refresh = true,
          }
        })
      end, 100)
    end,
  }



}


lvim.builtin.telescope.on_config_done = function(telescope)
  pcall(telescope.load_extension, "neoclip")
  pcall(telescope.load_extension("fzf"))
end



-- Autocommands (https://neovim.io/doc/user/autocmd.html)
-- vim.api.nvim_create_autocmd("BufEnter", {
--   pattern = { "*.json", "*.jsonc" },
--   -- enable wrap mode for json files only
--   command = "setlocal wrap",
-- })
-- vim.api.nvim_create_autocmd("FileType", {
--   pattern = "zsh",
--   callback = function()
--     -- let treesitter use bash highlight for zsh files as well
--     require("nvim-treesitter.highlight").attach(0, "bash")
--   end,
-- })
