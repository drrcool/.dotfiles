local status_ok, prettier = pcall(require, 'prettier')
if not status_ok then return end

-- prettier.setup({
--   ["null-ls"] = {
--     condition = function()
--       return prettier.config_exists({
--         check_package_json = true
--       })
--     end,
--     runtime_condition = function(params)
--       return true
--     end,
--     timeout = 5000
--   }
-- })
