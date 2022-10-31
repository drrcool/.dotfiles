local status_ok, navigator = pcall(require, 'navigator')
if not status_ok then
  print("issue with navigator")
  return
end

local M = {}
M.setup = function()
  navigator.setup({
    icons = {
      icons = true, -- set to false to use system default ( if you using a terminal does not have nerd/icon)
      -- code lens lf
      code_lens_action_icon = '',
      -- Diagnostics
      diagnostic_err = '📛',
      diagnostic_warn = '⚠️',
      diagnostic_info = [[ℹ️]],
      diagnostic_hint = [[🧩]],

      diagnostic_head_severity_2  = '☣️',
      diagnostic_head_severity_3  = '☢️',
      diagnostic_head_description = '👹',
      diagnostic_virtual_text     = '❕',
      diagnostic_file             = '🚑',
      -- Values
      value_changed               = '📝',
      value_definition            = '🍡', -- it is easier to see than 🦕
      side_panel                  = {
        section_separator = '',
        line_num_left = '',
        line_num_right = '',
        inner_node = '├○',
        outer_node = '╰○',
        bracket_left = '⟪',
        bracket_right = '⟫',
      },
      -- Treesitter
      match_kinds                 = {
        var = ' ', -- "👹", -- Vampaire
        method = 'ƒ ', --  "🍔", -- mac
        ['function'] = ' ', -- "🤣", -- Fun
        parameter = '  ', -- Pi
        associated = '🤝',
        namespace = '🚀',
        type = ' ',
        field = '🏈',
        module = '📦',
        flag = '🎏',
      },
      treesitter_defult           = '🌲',
      doc_symbols                 = '',
      code_action_icon            = "✎",
      diagnostic_head             = "",
      diagnostic_head_severity_1  = ""

    },
    lsp = {
      format_options = { async = false },
      disply_diagnostic_qf = false
    },

  })
end

return M
