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
