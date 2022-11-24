local swap_next, swap_prev = (function()
  local swap_objects = {
    p = '@parameter.inner',
    f = '@function.output',
    c = '@class.outer'
  }
  local n, p = {}, {}
  for key, obj in pairs(swap_objects) do
    n[string.format("<Leader>cx%s", key)] = obj
    p[string.format("<Leader>cX%s", key)] = obj
  end

  return n, p
end)()

lvim.builtin.autopairs.active = true
lvim.log.level = "warn"
lvim.format_on_save = true
lvim.transparent_window = true
vim.opt.relativenumber = true
lvim.builtin.autopairs.check_ts = true
lvim.builtin.nvimtree.setup.actions.open_file.quit_on_open = true
lvim.builtin.telescope.defaults.initial_mode = "insert"
lvim.builtin.treesitter.auto_install = true
lvim.builtin.treesitter.autotag.enable = true
lvim.builtin.treesitter.highlight.additional_vim_regex_highlighting = true
lvim.builtin.treesitter.highlight.enable = true
lvim.builtin.treesitter.indent.enable = true
lvim.builtin.treesitter.matchup.enable = true
lvim.builtin.treesitter.sync_install = false
lvim.builtin.treesitter.textobjects = {
  select = {
    enable = true,

    -- Automatically jump forward to textobj, similar to targets.vim
    lookahead = true,

    keymaps = {
      -- You can use the capture groups defined in textobjects.scm
      ["af"] = "@function.outer",
      ["if"] = "@function.inner",
      ["ac"] = "@class.outer",
      ["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
    },
    selection_modes = {
      ["@parameter.outer"] = "v", -- charwise
      ["@function.outer"] = "V", -- linewise
      ["@class.outer"] = "<c-v>", -- blockwise
    },
  },

  swap = {
    enable = true,
    swap_next = swap_next,
    swap_previous = swap_prev,
    -- swap_next = {
    --   ["<leader>cx"] = "@parameter.inner",
    -- },
    -- swap_previous = {
    --   ["<leader>cX"] = "@parameter.inner",
    -- },
  },

  move = {
    enable = true,
    set_jumps = true, -- whether to set jumps in the jumplist
    goto_next_start = {
      ["]m"] = "@function.outer",
      ["]]"] = "@class.outer",
    },
    goto_next_end = {
      ["]M"] = "@function.outer",
      ["]["] = "@class.outer",
    },
    goto_previous_start = {
      ["[m"] = "@function.outer",
      ["[["] = "@class.outer",
    },
    goto_previous_end = {
      ["[M"] = "@function.outer",
      ["[]"] = "@class.outer",
    },
  },

}
lvim.builtin.treesitter.textsubjects = {
  enable = true,
  prev_selection = ",", -- (Optional) keymap to select the previous selection
  keymaps = {
    ["."] = "textsubjects-smart",
    [";"] = "textsubjects-container-outer",
    ["i;"] = "textsubjects-container-inner",
  }
}
lvim.lsp.document_highlight = true
lvim.use_icons = true
