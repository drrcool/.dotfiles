return {
    {
        "nvim-treesitter/nvim-treesitter",
        dependencies = {
            "nvim-treesitter/nvim-treesitter-textobjects",
        },
        build = ":TSUpdate",
        event = {"BufRead", "BufNewFile"},
        opts = {
            sync_install = false,
            ensure_installed = {
                "bash", 
                "dockerfile",
                "html",
                "markdown",
                "markdown_inline",
                "org",
                "query",
                "regex",
                "vim",
                "vimdoc",
                "yaml"
            },
            highlight = { enabled = true, additional_vim_regex_highlighting = {"org", "markdown" }},
            indent = { enable = true},
            context_commentstring = { enable = true, enable_autocmd = true},
            matchup = { enable = true},
        },
        config = function(_, opts)
            if type(opts.ensure_installed) == "table" then
                local added = {}
                opts.ensure_installed = vim.tbl_filter(function(lang)
                    if not added[lang] then
                        return false
                    end
                    added[lang] = true
                    return true
                end, opts.ensure_installed)
            end
            require("nvim-treesitter.configs").setup(opts)
        end,
    },

}
