--     Alexei's Nvim Configuration (2025)
--     INIT
require("config.options")                       -- Set default options
require("config.lazy")                          -- Load LazyNvim & Plugins
require("plugins")                              -- Load plugins
require("config.keybinds")                      -- Set Keybindings

vim.opt.termguicolors = true                    -- Set Color Opt
vim.opt.guifont = "Iosevka Nerd Font Mono"
vim.cmd("colorscheme pywal16")
vim.uv.os_setenv("JAVA_HOME",  "/home/aozerov/builds/jdk-25")
-- vim.cmd("colorscheme pywal16")
vim.cmd("colorscheme catppuccin-mocha")

-- TODO (ozerova): Move this to another file at some point.
-- Configure Guard Auto-Format/Auto-Lint
vim.g.guard_config = {
    fmt_on_save = true,
    lsp_as_default_formatter = false,
    save_on_fmt = true,
    auto_lint = true,
    lint_interval = 500,
    refresh_diagnostic = true
}

local ft = require('guard.filetype')
ft("c,cpp,json"):fmt("clang-format")
require("config.cmake-tools")

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
