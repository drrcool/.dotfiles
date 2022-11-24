local setup_ok, snippy = pcall(require, 'snippy')
if not setup_ok then 
  print("Problem with snippy")
return
end


local M = {}


M.setup = function()
  snippy.setup({

    mappings = {
      is = {
        ['<Tab>'] = 'expand_or_advance',
        ['<S-Tab>'] = 'previous',
      },
      nx = {
        ['<leader>x'] = 'cut_text'
      }
    }
  })
end


return M
