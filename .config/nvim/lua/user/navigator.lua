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
      code_lens_action_icon = 'ī¤',
      -- Diagnostics
      diagnostic_err = 'đ',
      diagnostic_warn = 'â ī¸',
      diagnostic_info = [[âšī¸]],
      diagnostic_hint = [[đ§Š]],

      diagnostic_head_severity_2  = 'âŖī¸',
      diagnostic_head_severity_3  = 'âĸī¸',
      diagnostic_head_description = 'đš',
      diagnostic_virtual_text     = 'â',
      diagnostic_file             = 'đ',
      -- Values
      value_changed               = 'đ',
      value_definition            = 'đĄ', -- it is easier to see than đĻ
      side_panel                  = {
        section_separator = 'ī',
        line_num_left = 'î˛',
        line_num_right = 'î°',
        inner_node = 'ââ',
        outer_node = 'â°â',
        bracket_left = 'âĒ',
        bracket_right = 'âĢ',
      },
      -- Treesitter
      match_kinds                 = {
        var = 'î ', -- "đš", -- Vampaire
        method = 'Æ ', --  "đ", -- mac
        ['function'] = 'ī ', -- "đ¤Ŗ", -- Fun
        parameter = 'ī  ', -- Pi
        associated = 'đ¤',
        namespace = 'đ',
        type = 'ī  ',
        field = 'đ',
        module = 'đĻ',
        flag = 'đ',
      },
      treesitter_defult           = 'đ˛',
      doc_symbols                 = 'î',
      code_action_icon            = "â",
      diagnostic_head             = "îĸ",
      diagnostic_head_severity_1  = "ī"

    },
    lsp = {
      format_options = { async = false },
      disply_diagnostic_qf = false
    },

  })
end

return M
