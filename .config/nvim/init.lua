require("plugins/deps")
local add, now, later = MiniDeps.add, MiniDeps.now, MiniDeps.later

--          ╔═════════════════════════════════════════════════════════╗
--          ║                          MVIM                           ║
--          ╚═════════════════════════════════════════════════════════╝

require("config/options")       -- Set basic NeoVim configurations
require("plugins/neovide")      -- Configure Neovide 
require("plugins/clue")         -- Configure Mini.Clue
require("plugins/essentials")   -- Configure essentials (Align, Animate, Basics, Bracketed, 
                                --      Bufremove, Colors, Comment, Completion, CursorWord, 
                                --      Doc, Extra, Files)
require("plugins/patterns")     -- Pattern matching for highlighting and redacting passwords
require("plugins/todo")         -- TODO: Complete sorting this later
require("plugins/starter")      -- TODO: Review what this is
require("plugins/treesitter")   -- Enable treesitter

require("autocmds")
require("filetypes")
require("highlights")
require("keybinds")
require("user-plugins")

--          ╔═════════════════════════════════════════════════════════╗
--          ║                          LSP                            ║
--          ╚═════════════════════════════════════════════════════════╝

vim.lsp.enable("lua_ls")
vim.lsp.enable("rust_analyzer")
