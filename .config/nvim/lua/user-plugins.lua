--          ╭─────────────────────────────────────────────────────────╮
--          │                     Neovim Plugins                      │
--          ╰─────────────────────────────────────────────────────────╯

local add, now, later = MiniDeps.add, MiniDeps.now, MiniDeps.later

--     Neotree
add({
    source = 'nvim-neo-tree/neo-tree.nvim',
    depends = {
        "nvim-lua/plenary.nvim",
        "nvim-tree/nvim-web-devicons",
        "MunifTanjim/nui.nvim",
    }
})

--     Pywal16 Theme

add({
    source = 'uZer/pywal16.nvim'
})
local pywal16 = require('pywal16')
pywal16.setup()

--     Zenbones Theme
add({
   source = "zenbones-theme/zenbones.nvim",
   depends = {
       "rktjmp/lush.nvim"
   }
})


-- Enable Colorscheme
now(function()
    vim.cmd("colorscheme zenbones")
end)

-- LSP Config 
add({
    source = 'neovim/nvim-lspconfig'
})

-- Rustacean
add({
    source = 'mrcjkb/rustaceanvim'
})
