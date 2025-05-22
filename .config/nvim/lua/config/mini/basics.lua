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
