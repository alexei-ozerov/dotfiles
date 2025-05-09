local add, now, later = MiniDeps.add, MiniDeps.now, MiniDeps.later

later(function()
    require("mini.fuzzy").setup()
end)

later(function()
    require("mini.icons").setup()
end)
later(function()
    require("mini.indentscope").setup({
        draw = {
            animation = function()
                return 1
            end,
        },
        symbol = "│",
    })
end)
later(function()
    require("mini.jump").setup()
end)
later(function()
    require("mini.jump2d").setup()
end)
later(function()
    require("mini.map").setup()
end)
later(function()
    require("mini.misc").setup()
end)
later(function()
    require("mini.move").setup({
        mappings = {
            -- Move visual selection in Visual mode. Defaults are Alt (Meta) + hjkl.
            left = '<S-h>',
            right = '<S-l>',
            down = '<S-j>',
            up = '<S-k>',

            -- Move current line in Normal mode
            line_left = '<S-h>',
            line_right = '<S-l>',
            line_down = '<S-j>',
            line_up = '<S-k>',
        },
    }
    )
end)
later(function()
    --          ┌─────────────────────────────────────────────────────────┐
    --            We took this from echasnovski's personal configuration
    --           https://github.com/echasnovski/nvim/blob/master/init.lua
    --          └─────────────────────────────────────────────────────────┘
    local filterout_lua_diagnosing = function(notif_arr)
        local not_diagnosing = function(notif)
            return not vim.startswith(notif.msg, "lua_ls: Diagnosing")
        end
        notif_arr = vim.tbl_filter(not_diagnosing, notif_arr)
        return MiniNotify.default_sort(notif_arr)
    end
    require("mini.notify").setup({
        content = { sort = filterout_lua_diagnosing },
        window = { config = { border = "solid" } },
    })
    vim.notify = MiniNotify.make_notify()
end)
later(function()
    require("mini.operators").setup()
end)
later(function()
    require("mini.pairs").setup()
end)
later(function()
    local win_config = function()
        local height = math.floor(0.618 * vim.o.lines)
        local width = math.floor(0.4 * vim.o.columns)
        return {
            anchor = "NW",
            height = height,
            width = width,
            border = "solid",
            row = math.floor(0.5 * (vim.o.lines - height)),
            col = math.floor(0.5 * (vim.o.columns - width)),
        }
    end
    require("mini.pick").setup({
        mappings = {
            choose_in_vsplit = "<C-CR>",
        },
        options = {
            use_cache = true,
        },
        window = {
            config = win_config,
        },
    })
    vim.ui.select = MiniPick.ui_select
end)
now(function()
    require("mini.sessions").setup({
        autowrite = true,
    })
end)
later(function()
    require("mini.splitjoin").setup()
end)
later(function()
    local gen_loader = require('mini.snippets').gen_loader
    require('mini.snippets').setup({
        snippets = {
            -- Load custom file with global snippets first (adjust for Windows)
            gen_loader.from_file('~/.config/nvim/snippets/global.json'),

            -- Load snippets based on current language by reading files from
            -- "snippets/" subdirectories from 'runtimepath' directories.
            gen_loader.from_lang(),
        },
    })
end)
later(function()
    require("mini.statusline").setup()
end)
later(function()
    require("mini.surround").setup()
end)
later(function()
    require("mini.tabline").setup()
end)
later(function()
    require("mini.trailspace").setup()
end)
later(function()
    require("mini.visits").setup()
end)

