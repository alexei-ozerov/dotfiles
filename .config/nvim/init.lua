require("config.options")        -- Set default options
require("config.lazy")           -- Load LazyNvim & Plugins
require("config.keybinds")       -- Set Keybindings

--     MINI.NVIM
require("config.mini-starter")   -- Load Mini.Starter

require("mini.align").setup()
require("mini.bracketed").setup()
require("mini.bufremove").setup()
require("mini.comment").setup()
require("mini.cursorword").setup()
require("mini.extra").setup()
require("mini.doc").setup()
require("mini.statusline").setup()

require("config.mini-basics")    -- Load various Mini Modules
require("config.mini-animate")
require("config.mini-completion")
require("config.mini-diff")
require("config.mini-move")      -- Load Mini.Move (buffer manipulation)
require("config.mini-clue")      -- Load Mini.Clue (hints)
require("config.mini-pairs")     -- Load Mini.Pairs (auto pairs)

--     TREESITTER
require("config.treesitter")
