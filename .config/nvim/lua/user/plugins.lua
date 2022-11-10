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

  ---------------------------------
  ---- General Plugins           --
  ---------------------------------
  ---- Faster Loading
  use({ "lewis6991/impatient.nvim",
    config = function()
      require('user.impatient')
    end })

  use({ "ton/vim-bufsurf" })





  --  Status Line
  use({ "akinsho/bufferline.nvim",
    config = function()
      require("user.bufferline")
    end,
  })
  use({ 'alvarosevilla95/luatab.nvim', requires = 'kyazdani42/nvim-web-devicons',
    config = function()
      require('user.luatab').setup()
    end
  })

  use({ "nvim-lualine/lualine.nvim",
    config = function()
      require("user.lualine")
    end,
  })

  ---- Icons
  --use({ "kyazdani42/nvim-web-devicons" })

  -- color
  use({ "EdenEast/nightfox.nvim" })
  use({ "catppuccin/nvim", as = "catppuccin" })
  use({ "folke/tokyonight.nvim" })
  use({ "norcalli/nvim-colorizer.lua",
    config = function()
      require("user.colorizer")
    end,
  })
  use({ "nanotech/jellybeans.vim" })
  use({ "bluz71/vim-moonfly-colors" })
  use({ "bignimbus/pop-punk.vim" })
  use({ "krfl/fleetish-vim" })
  use({ "tomasr/molokai" })

  -- -- UI
  use({ "gennaro-tedesco/nvim-peekup" })
  use({ "goolord/alpha-nvim",
    config = function()
      require("user.alpha")
    end,
  })
  -- nav
  use({ "folke/which-key.nvim",
    config = function()
      require('user.whichkey')
    end
  })
  use({ "mrjones2014/legendary.nvim" })
  use({ "karb94/neoscroll.nvim", config = function()
    require('user.neoscroll')

  end })
  -- -- Editor Utils
  use({ "nvim-treesitter/nvim-treesitter",
    config = function()
      require('user.treesitter')
    end,
  })
  use({ "lukas-reineke/indent-blankline.nvim",
    config = function()
      require('user.indentline')
    end
  })
  use({ "simrat39/symbols-outline.nvim",
    config = function()
      require('user.outline')
    end
  })
  use({ "windwp/nvim-autopairs",
    config = function()
      require('user.autopairs')
    end

  }) -- Autopairs, integrates with both cmp and treesitter
  use({ 'tpope/vim-commentary' })
  use({ "JoosepAlviste/nvim-ts-context-commentstring" })
  use({ "simrat39/inlay-hints.nvim",
    config = function()
      require('user.inlay-hints')
    end

  })
  use({ "RRethy/vim-illuminate", config = function() require('user.illuminate') end })
  use({ 'kevinhwang91/nvim-ufo', requires = 'kevinhwang91/promise-async',
    config = function()
      require('user.ufo')
    end
  })

  use({ "p00f/nvim-ts-rainbow" })

  use({
    "folke/todo-comments.nvim",
    requires = "nvim-lua/plenary.nvim",
    config = function()
      require("user.todo")
    end,
  })
  --
  use({ "ahmedkhalf/project.nvim",
    config = function()
      require('user.project')
    end
  })


  ---- -- Misc utils
  use({ "nvim-lua/plenary.nvim" }) -- Useful lua functions used by lots of plugins
  use({ "akinsho/toggleterm.nvim",
    config = function()
      require('user.toggleterm')
    end
  })



  use({ "moll/vim-bbye" })
  use({ "tiagovla/scope.nvim",
    config = function()
      require('user.scope')
    end
  })
  use({ 'ggandor/lightspeed.nvim' })
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
    config = function()
      require('user.neoclip')
    end
  })
  use({ "nvim-telescope/telescope-file-browser.nvim" })
  use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })

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
  use({ "hrsh7th/cmp-omni" })
  use({ "hrsh7th/cmp-cmdline" }) -- cmdline completions
  use({ "hrsh7th/cmp-emoji" })
  use({ "hrsh7th/cmp-nvim-lsp" })
  use({ 'b0o/SchemaStore.nvim' })
  use({ "hrsh7th/cmp-nvim-lua", ft = { 'lua' } })
  use({ "hrsh7th/cmp-path" }) -- path completions
  use({ "hrsh7th/nvim-cmp" }) -- The completion plUIgin
  use({ "saadparwaiz1/cmp_luasnip" }) -- snippet completions
  use({ "onsails/lspkind.nvim" })
  -- Snippets
  use({ "L3MON4D3/LuaSnip" }) --snippet engine
  use({ "rafamadriz/friendly-snippets" }) -- a bunch of snippets to use
  -- Telescope
  use({ "nvim-telescope/telescope.nvim",
    config = function()
      require('user.telescope')
    end
  })



  -- -------------------------------
  -- -- Development Plugins --
  -- -------------------------------
  -- LSP

  use({ 'williamboman/mason.nvim',
    config = function()
      require('mason').setup()
    end })
  use({ 'jose-elias-alvarez/null-ls.nvim' })
  use({ "jayp0521/mason-null-ls.nvim" })
  use({ "RubixDev/mason-update-all" })
  use({ "williamboman/mason-lspconfig.nvim" })

  use("neovim/nvim-lspconfig")
  use({ 'lukas-reineke/lsp-format.nvim' })


  use "Pocco81/DAPInstall.nvim"

  use({
    "folke/trouble.nvim",
    requires = "kyazdani42/nvim-web-devicons",
    config = function()
      require('user.trouble')
    end,
  })
  use {
    "ThePrimeagen/refactoring.nvim",
    requires = {
      { "nvim-lua/plenary.nvim" },
      { "nvim-treesitter/nvim-treesitter" }
    },
    config = function()
      require('user.refactor')
    end
  }
  -- -- Git
  use({ "lewis6991/gitsigns.nvim",
    config = function()
      require('user.gitsigns')
    end
  })
  use({ "f-person/git-blame.nvim" })
  use({ "ruifm/gitlinker.nvim" })
  use({ "mattn/vim-gist" })
  use({ "mattn/webapi-vim" })
  use({ "sindrets/diffview.nvim", requires = "nvim-lua/plenary.nvim" })
  use({
    "microsoft/vscode-js-debug",
    run = "npm install --legacy-peer-deps && npm run compile"
  })
  -- -- Debugging
  use {
    "mfussenegger/nvim-dap",
    module = { "dap" },
    requires = {
      "theHamsta/nvim-dap-virtual-text",
      "rcarriga/nvim-dap-ui",
      "mfussenegger/nvim-dap-python",
      "nvim-telescope/telescope-dap.nvim",
      { "leoluz/nvim-dap-go", module = "dap-go" },
      { "jbyuki/one-small-step-for-vimkind", module = "osv" },
      { "mxsdev/nvim-dap-vscode-js" },
      {
        "microsoft/vscode-js-debug",
        run = "npm install --legacy-peer-deps && npm run compile",
      },
    },
    config = function()
      require("config.dap").setup()
    end,
    disable = false,
  }
  --
  --
  -- -- Testing
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
    config = function()
      require("user.neotest")
    end,
  })
  --
  -- -- Task Runner
  use({ 'stevearc/overseer.nvim',
    config = function()
      require('overseer').setup()
    end
  })
  --

  use({ 'edluffy/specs.nvim',
    config = function()
      require('user.specs').setup()
    end })

  --
  -- -------------------------------
  -- -- Language Specific Plugins --
  -- -------------------------------
  -- Typescript
  use({ "jose-elias-alvarez/typescript.nvim",
    config = function()
      require("typescript").setup({})
    end })
  use('tpope/vim-repeat')
  use('tpope/vim-eunuch')
  use('romainl/vim-cool')
  use('AndrewRadev/tagalong.vim')
  use('alvan/vim-closetag')
  use({ 'djoshea/vim-autoread' })
  ----------------
  -- -- Package.json npm dependencies
  -- --------------
  use({ "vuki656/package-info.nvim",
    requires = "MunifTanjim/nui.nvim",
    config = function()
      require("user.package-info").setup()
    end,
  })

  use({
    "folke/noice.nvim",
    config = function()
      require("user.noice").setup()
    end,
    requires = {
      "MunifTanjim/nui.nvim",
      "rcarriga/nvim-notify"
    },
    disable = false
  })

  use({ "rcarriga/nvim-notify",
    config = function()
      require("user.notify")
    end,
    disable = false })
  use({
    "folke/twilight.nvim",
    config = function()
      require("twilight").setup()
    end,
  })
  -- Harpoon
  use({ "ThePrimeagen/harpoon" })

  -- nvim tree
  use({
    "kyazdani42/nvim-tree.lua",
    requires = "kyazdani42/nvim-web-devicons",
    config = function()
      require('user.nvim-tree').setup()

    end
  })

  use({
    "ThePrimeagen/git-worktree.nvim",
    config = function()
      require('git-worktree').setup({
        autopush = true,

      })
    end
  })
  use({ 'kevinhwang91/nvim-bqf',
    config = function()
      require('user.bqf')
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
