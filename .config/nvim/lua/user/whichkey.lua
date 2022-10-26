local status_ok, which_key = pcall(require, "which-key")
if not status_ok then
  return
end

require('legendary').setup()

local setup = {
  plugins = {
    marks = true, -- shows a list of your marks on ' and `
    registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
    spelling = {
      enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
      suggestions = 20, -- how many suggestions should be shown in the list?
    },
    -- the presets plugin, adds help for a bunch of default keybindings in Neovim
    -- No actual key bindings are created
    presets = {
      operators = false, -- adds help for operators like d, y, ... and registers them for motion / text object completion
      motions = true, -- adds help for motions
      text_objects = true, -- help for text objects triggered after entering an operator
      windows = true, -- default bindings on <c-w>
      nav = true, -- misc bindings to work with windows
      z = true, -- bindings for folds, spelling and others prefixed with z
      g = true, -- bindings for prefixed with g
    },
  },
  -- add operators that will trigger motion and text object completion
  -- to enable all native operators, set the preset / operators plugin above
  -- operators = { gc = "Comments" },
  key_labels = {
    -- override the label used to display some keys. It doesn't effect WK in any other way.
    -- For example:
    -- ["<space>"] = "SPC",
    -- ["<cr>"] = "RET",
    -- ["<tab>"] = "TAB",
  },
  icons = {
    breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
    separator = "➜", -- symbol used between a key and it's label
    group = "+", -- symbol prepended to a group
  },
  popup_mappings = {
    scroll_down = "<c-d>", -- binding to scroll down inside the popup
    scroll_up = "<c-u>", -- binding to scroll up inside the popup
  },
  window = {
    border = "rounded", -- none, single, double, shadow
    position = "bottom", -- bottom, top
    margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
    padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
    winblend = 0,
  },
  layout = {
    height = { min = 4, max = 25 }, -- min and max height of the columns
    width = { min = 20, max = 50 }, -- min and max width of the columns
    spacing = 3, -- spacing between columns
    align = "left", -- align columns left, center or right
  },
  ignore_missing = true, -- enable this to hide mappings for which you didn't specify a label
  hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " }, -- hide mapping boilerplate
  show_help = true, -- show help message on the command line when the popup is visible
  triggers = "auto", -- automatically setup triggers
  -- triggers = {"<leader>"} -- or specify a list manually
  triggers_blacklist = {
    -- list of mode / prefixes that should never be hooked by WhichKey
    -- this is mostly relevant for key maps that start with a native binding
    -- most people should not need to change this
    i = { "j", "k" },
    v = { "j", "k" },
  },
}

local opts = {
  mode = "n", -- NORMAL mode
  prefix = "<leader>",
  buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
  silent = true, -- use `silent` when creating keymaps
  noremap = true, -- use `noremap` when creating keymaps
  nowait = true, -- use `nowait` when creating keymaps
}

local mappings = {
  ["L"] = { "<cmd>Legendary<cr>", "Legendary" },
  ["C"] = { "<cmd>lua require('telescope').extensions.neoclip.default()<cr>", "Neoclip" },
  ["a"] = { "<cmd>Alpha<cr>", "Alpha" },
  ["e"] = { "<cmd>Neotree float reveal_force_cwd<cr>", "Explorer" },
  ["w"] = { "<cmd>w!<CR>", "Save" },
  ["q"] = { "<cmd>q!<CR>", "Quit" },
  ["c"] = { "<cmd>Bdelete!<CR>", "Close Buffer" },
  ["h"] = { "<cmd>nohlsearch<CR>", "No Highlight" },
  ["P"] = { "<cmd>lua require('telescope').load_extension('projects').projects()<cr>", "Projects" },
  ["R"] = { "<cmd>source $MYVIMRC<cr>", "Reload" },
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
    r = {
      '<cmd>lua require\'telescope\'.extensions.file_browser.file_browser()<cr>'
      ,
      'File Browser',
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

local vopts = {
  mode = "v", -- VISUAL mode
  prefix = "<leader>",
  buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
  silent = true, -- use `silent` when creating keymaps
  noremap = true, -- use `noremap` when creating keymaps
  nowait = true, -- use `nowait` when creating keymaps
}

local vmappings = {
  ["R"] = { "<cmd>lua require('telescope').load_extension('refactoring').refactors()<cr>", "Refactor" },
  r = {
    name = 'Refactor',
    f = {
      [[<cmd>lua require('refactoring').refactor('Extract Function')<cr>]],
      'Extract Function',
    },
    F = {
      [[ <cmd>lua require('refactoring').refactor('Extract Function to File')<cr>]],
      'Extract Function to File',
    },
    v = {
      [[<cmd>lua require('refactoring').refactor('Extract Variable')<cr>]],
      'Extract Variable',
    },
    i = {
      [[<cmd>lua require('refactoring').refactor('Inline Variable')<cr>]],
      'Inline Variable',
    },
    r = {
      [[<cmd>lua require('telescope').extensions.refactoring.refactors()<cr>]],
      'Refactor',
    },
    d = {
      [[<cmd>lua require('refactoring').debug.print_var({})<cr>]],
      'Debug Print Var',
    },
  }
}

which_key.setup(setup)
which_key.register(mappings, opts)
which_key.register(vmappings, vopts)
--local l_status_ok, legendary = pcall(require, "legendary")
--if not l_status_ok then
--  return
--end
--
--legendary.setup({
--  which_key = {
--    mappings = mappings,
--    opts = opts,
--    do_binding = false,
--  },
--})
