--     Alexei's Nvim Configuration (2025)
--     INIT
require("config.options")                       -- Set default options
require("config.lazy")                          -- Load LazyNvim & Plugins
require("plugins")
require("config.keybinds")                      -- Set Keybindings

vim.opt.termguicolors = true                    -- Set Color Opt
vim.opt.guifont = "Iosevka Nerd Font Mono"
vim.cmd("colorscheme pywal16")

--     MINI.NVIM
require("config.mini.starter")                  -- Load Mini.Starter

-- Default Configuration
require("mini.align").setup()
require("mini.bracketed").setup()
require("mini.bufremove").setup()
require("mini.comment").setup()
require("mini.cursorword").setup()
require("mini.extra").setup()
require("mini.doc").setup()
require("mini.statusline").setup()
require("mini.icons").setup()
require("mini.notify").setup()
require("mini.tabline").setup()
require("mini.files").setup()
require("mini.pick").setup()
require("mini.sessions").setup()
require("mini.snippets").setup()
require("mini.git").setup()
require("mini.diff").setup()
-- require("mini.indentscope").setup()

-- Additional Configuration
-- Look under ./lua/config/mini/.
require("config.mini.basics")                   -- Load Mini.Basics (defaults)
require("config.mini.animate")                  -- Load Mini.Animate (basic animations)
-- require("config.mini.completion")            -- Load Mini.Completion (completion library)
require("config.mini.diff")                     -- Load Mini.Diff (diff gutter status)
require("config.mini.move")                     -- Load Mini.Move (buffer manipulation)
require("config.mini.clue")                     -- Load Mini.Clue (hints)
require("config.mini.pairs")                    -- Load Mini.Pairs (auto pairs)

--     TREESITTER
require("config.treesitter")
