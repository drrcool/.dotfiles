local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  PACKER_BOOTSTRAP = fn.system({
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  })
  print("Installing packer close and reopen Neovim...")
  vim.cmd([[packadd packer.nvim]])
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]])

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
  return
end

-- Have packer use a popup window
packer.init({
  display = {
    open_fn = function()
      return require("packer.util").float({ border = "rounded" })
    end,
  },
})

return packer.startup({ function(use)
  use({ "wbthomason/packer.nvim" }) -- Have packer manage itself

  -------------------------------
  -- General Plugins           --
  -------------------------------
  -- Appearance
  use({ "akinsho/bufferline.nvim" })
  use({ "kyazdani42/nvim-web-devicons" })
  use({ "nvim-lualine/lualine.nvim" })
  use({ "altercation/vim-colors-solarized" })
  use({ "EdenEast/nightfox.nvim" })
  -- color
  use({ "folke/tokyonight.nvim" })
  use({ "tiagovla/tokyodark.nvim" })
  use({ "vimpostor/vim-tpipeline" })
  use({ "norcalli/nvim-colorizer.lua" })
  use({ "nanotech/jellybeans.vim" })
  use({ "bluz71/vim-moonfly-colors" })
  use({ "rgzee/dark.nvim" })
  use({ 'lunarvim/darkplus.nvim' })
  use({ "LunarVim/Colorschemes" })
  use({ "NLKNguyen/papercolor-theme" })
  use({ "bignimbus/pop-punk.vim" })
  use({ "krfl/fleetish-vim" })
  use({ "tomasr/molokai" })
  use({ "w0ng/vim-hybrid" })

  -- UI
  use({ "gennaro-tedesco/nvim-peekup" })
  use({ "stevearc/dressing.nvim" })
  use({ "goolord/alpha-nvim" })
  use({ "rcarriga/nvim-notify" })
  --[[ use({ "rcarriga/nvim-notify" }) ]]
  use({
    "Pocco81/true-zen.nvim",
    config = function()
      require("true-zen").setup {}
    end,
  })

  -- Navigation
  use({ "folke/which-key.nvim" })
  use({ "mrjones2014/legendary.nvim" })
  use({ "karb94/neoscroll.nvim" })
  -- Editor Utils
  use({ "nvim-treesitter/nvim-treesitter" })
  use({ "lukas-reineke/indent-blankline.nvim" })
  use({ "simrat39/symbols-outline.nvim" })
  use({ "windwp/nvim-autopairs" }) -- Autopairs, integrates with both cmp and treesitter
  use({ "JoosepAlviste/nvim-ts-context-commentstring" })
  use({ "numToStr/Comment.nvim" })
  use({ "simrat39/inlay-hints.nvim" })
  use({ "RRethy/vim-illuminate" })
  use({ 'kevinhwang91/nvim-ufo', requires = 'kevinhwang91/promise-async' })
  use({ "p00f/nvim-ts-rainbow" })
  use({ "itchyny/vim-highlighturl", event = "BufRead" })
  -- Lua
  use({
    "folke/todo-comments.nvim",
    requires = "nvim-lua/plenary.nvim",
  })

  -- Projects / Sessions
  use({ "folke/persistence.nvim", event = "BufReadPre", module = "persistence" })
  use({ "ahmedkhalf/project.nvim" })

  -- Misc utils
  use({ "nvim-lua/plenary.nvim" }) -- Useful lua functions used by lots of plugins
  use({ "akinsho/toggleterm.nvim" })
  use({ "lewis6991/impatient.nvim" })
  use({ "moll/vim-bbye" })
  use({ "tiagovla/scope.nvim" })
  use({
    "sudormrfbin/cheatsheet.nvim",
    requires = {
      { "nvim-telescope/telescope.nvim" },
      { "nvim-lua/popup.nvim" },
      { "nvim-lua/plenary.nvim" },
    },
  })
    config = function()
  use({ "ggandor/leap.nvim",
    config = function()

      require('leap').add_default_mappings()

    end
  })

  use({
    "wakatime/vim-wakatime",
  })
  use({
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
  })


  use({ "kkharji/sqlite.lua" })
  use({
    "AckslD/nvim-neoclip.lua",
    requires = {
      { "kkharji/sqlite.lua", module = "sqlite" },
      { "nvim-telescope/telescope.nvim" },
    },
  })
  use({ "nvim-telescope/telescope-file-browser.nvim" })
  use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
  use({ "fhill2/telescope-ultisnips.nvim" })

  use({ "github/copilot.vim" })
  use {
    "zbirenbaum/copilot-cmp",
    after = { "copilot.lua" },
    config = function()
      require("copilot_cmp").setup()
    end
  }
  use {
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

  -- Completion plugins
  use({ "hrsh7th/cmp-buffer" }) -- buffer completions
  use({ "hrsh7th/cmp-cmdline" }) -- cmdline completions
  use({ "hrsh7th/cmp-emoji" })
  use({ "hrsh7th/cmp-nvim-lsp" })
  use({ "hrsh7th/cmp-nvim-lua" })
  use({ "hrsh7th/cmp-path" }) -- path completions
  use({ "hrsh7th/nvim-cmp" }) -- The completion plugin
  use({ "saadparwaiz1/cmp_luasnip" }) -- snippet completions
  use({ "onsails/lspkind.nvim" })
  -- Snippets
  use({ "L3MON4D3/LuaSnip" }) --snippet engine
  use({ "rafamadriz/friendly-snippets" }) -- a bunch of snippets to use

  -- Telescope
  use({ "nvim-telescope/telescope.nvim" })

  -------------------------------
  -- Development Plugins --
  -------------------------------
  -- LSP
  use({
    'ray-x/navigator.lua',
    requires = {
      { 'ray-x/guihua.lua', run = 'cd lua/fzy && make' },
      { 'neovim/nvim-lspconfig' },
    },
    config = function()
      require 'navigator'.setup()
    end
  })

  use({
    "folke/trouble.nvim",
    requires = "kyazdani42/nvim-web-devicons",
  })
  use {
    "ThePrimeagen/refactoring.nvim",
    requires = {
      { "nvim-lua/plenary.nvim" },
      { "nvim-treesitter/nvim-treesitter" }
    }
  }
  -- Git
  use({ "lewis6991/gitsigns.nvim" })
  use({ "f-person/git-blame.nvim" })
  use({ "ruifm/gitlinker.nvim" })
  use({ "mattn/vim-gist" })
  use({ "mattn/webapi-vim" })
  use({ "sindrets/diffview.nvim", requires = "nvim-lua/plenary.nvim" })

  -- Debug
  use({ "mfussenegger/nvim-dap" })
  use({ "rcarriga/nvim-dap-ui" })
  use({ "theHamsta/nvim-dap-virtual-text" })
  use({
    "nvim-neotest/neotest",
    requires = {
      "nvim-lua/plenary.nvim",
      "vim-test/vim-test",
      "nvim-treesitter/nvim-treesitter",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-neotest/neotest-python",
      "haydenmeade/neotest-jest",
    },
  })
  use({ 'Pocco81/DAPInstall.nvim' })

  -- Task Runner
  use({ 'stevearc/overseer.nvim',
    config = function()
      require('overseer').setup()
    end
  })




  -------------------------------
  -- Language Specific Plugins --
  -------------------------------
  -- Typescript
  use({ "jose-elias-alvarez/typescript.nvim" })
  use({
    "microsoft/vscode-js-debug",
    opt = true,
    run = "npm install --legacy-peer-deps && npm run compile"
  })
  use({
    "mxsdev/nvim-dap-vscode-js",
    requires = { "mfussenegger/nvim-dap" }
  })
  use({ "MunifTanjim/eslint.nvim" })
  use({ "MunifTanjim/prettier.nvim" })


  ---------------
  -- Package.json npm dependencies
  --------------
  use({ "vuki656/package-info.nvim",
    requires = "MunifTanjim/nui.nvim",
    config = function()
      require("user.package-info").setup()
    end,
  })

  -- Harpoon
  use({ "ThePrimeagen/harpoon" })

  -- Undo
  use({ "mbbill/undotree", cmd = { "UndoTreeToggle" } })
  --- Better quickfix
  use({
    "https://gitlab.com/yorickpeterse/nvim-pqf.git",
    config = function()
      require('pqf').setup()
    end
  })
  -- nvim tree
  use({
    "kyazdani42/nvim-tree.lua",
    requires = "kyazdani42/nvim-web-devicons",
    config = function()
      require('user.nvimtree').setup()
    end
  })




  if PACKER_BOOTSTRAP then
    require("packer").sync()
  end
end,
  config = {
    max_jobs = 10
  },
})
