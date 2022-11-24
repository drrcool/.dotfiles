--[[ 
lvim is the global options object

Linters should bejj
filled in as strings with either
a global executable or a path to
an executable
]]
-- THESE ARE EXAMPLE CONFIGS FEEL FREE TO CHANGE TO WHATEVER YOU WANT
require('user.core.keymaps')
require('user.core.whichkey')

-- general
require 'user.update_builtin_settings'
-- TODO: User Config for predefined plugins
-- After changing plugin config exit and reopen LunarVim, Run :PackerInstall :PackerCompile
lvim.builtin.alpha.active = true
lvim.builtin.alpha.mode = "dashboard"
lvim.builtin.terminal.active = true
lvim.builtin.nvimtree.setup.view.side = "right"
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
  --- Treesitter-enabled
  { 'chilicuil/nextCS' },
  { 'shaeinst/roshnivim-cs' },
  { 'rafamadriz/neon' },
  { 'tomasiser/vim-code-dark' },
  { 'Mofiqul/vscode.nvim' },
  { 'marko-cerovac/material.nvim' },
  { 'bluz71/vim-nightfly-colors' },
  { 'bluz87/vim-moonfly-colors' },
  { 'ChristianChiarulli/nvcode-color-schemes.vim' },
  { 'folke/tokyonight.nvim' },
  { 'sainnhe/sonokai' },
  { 'kyazdani82/blue-moon' },
  { 'hmartington/oceanic-next' },
  { 'glepnir/zephyr.nvim' },
  { 'rockerBOO/boo-colorscheme-nvim' },
  { 'jim-at-jibba/ariake-vim-colors' },
  { 'Th3Whit3Wolf/onebuddy' },
  { 'ishan9299/modus-theme-vim' },
  { 'sainne/edge' },
  { "theniceboy/nvim-deus" },
  { "bkegley/gloombuddy" },
  { "Th3Whit3Wolf/one-nvim" },
  { "PHSix/nvim-hybrid" },
  { "Th3Whit3Wolf/space-nvim" },
  { "yonlu/omni.vim" },
  { "ray-x/aurora" },
  { "ray-x/starry.nvim" },
  { 'tanvirthin/monokai.nvim' },
  { 'ofirgall/ofirkai.nvim' },
  { 'savq/melange' },
  { 'RRethy/nvim-base-16' },
  { 'fenetikm/falcon' },
  { 'andersevenrud/nordic.nvim' },
  { 'shaunsingh/nord.nvim' },
  { 'shaunsingh/moonlight.nvim' },
  { 'NTBBloodbath/doom-one.nvim' },
  { 'dracula/vim' },
  { 'yashguptaz/calvera-dark.nvim' },
  { 'nxvu699134/vn-night.nvim' },
  { 'adisen99/codeschool.nvim' },
  { 'mcchrish/zenbones.nvim' },
  { 'FrenzyExists/aquarium-vim' },
  { 'kvrohit/substrata.nvim' },
  { 'titanzero/zephyrium' },
  { 'tiagovla/tokyodark.nvim' },
  { 'rockyzhang24/arctic.nvim' },
  { 'lewpoly/sherbet.nvim' },
  { 'metalelf0/jellybeans-nvim' },


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
    end,
  },
  { 'tpope/vim-commentary' },
  { 'ThePrimeagen/git-worktree.nvim',
    config = function()
      require('user.git-worktree')
    end
  },
  { "ThePrimeagen/harpoon" },
  { 'nvim-treesitter/nvim-treesitter-textobjects' },
  { 'RRethy/nvim-treesitter-textsubjects' },
  { 'rktjmp/lush.nvim' },
  { 'tjdevries/colorbuddy.nvim' },
  { 'jdhao/better-escape.vim' }
}


lvim.builtin.telescope.on_config_done = function(telescope)
  pcall(telescope.load_extension, 'git_worktree')
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
