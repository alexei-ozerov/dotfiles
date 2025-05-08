local add, now, later = MiniDeps.add, MiniDeps.now, MiniDeps.later

later(function()
    require("mini.align").setup()
end)
later(function()
    local animate = require("mini.animate")
    animate.setup({
        scroll = {
            -- Disable Scroll Animations, as the can interfer with mouse Scrolling
            enable = false,
        },
        cursor = {
            timing = animate.gen_timing.cubic({ duration = 50, unit = "total" }),
        },
    })
end)

later(function()
    require("mini.basics").setup({
        options = {
            basic = true,
            extra_ui = true,
            win_borders = "bold",
        },
        mappings = {
            basic = true,
            windows = true,
        },
        autocommands = {
            basic = true,
            relnum_in_visual_mode = true,
        },
    })
end)
later(function()
    require("mini.bracketed").setup()
end)
later(function()
    require("mini.bufremove").setup()
end)
later(function() require('mini.colors').setup() end)
later(function()
    require("mini.comment").setup()
end)
later(function()
    require("mini.completion").setup({
        mappings = {
            go_in = "<RET>",
        },
        window = {
            info = { border = "solid" },
            signature = { border = "solid" },
        },
    })
end)
later(function()
    require("mini.cursorword").setup()
end)
later(function()
    require("mini.diff").setup({
        view = {
            style = "sign",
            signs = { add = "█", change = "▒", delete = "" },
        },
    })
end)
later(function()
    require("mini.doc").setup()
end)
later(function()
    require("mini.extra").setup()
end)
later(function()
    require("mini.files").setup({
        mappings = {
            close = '<ESC>',
        },
        windows = {
            preview = true,
            border = "solid",
            width_preview = 80,
        },
    })
end)

