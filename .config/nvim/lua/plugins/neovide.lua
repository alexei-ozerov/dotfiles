local add, now, later = MiniDeps.add, MiniDeps.now, MiniDeps.later

--          ╭─────────────────────────────────────────────────────────╮
--          │                         Neovide                         │
--          ╰─────────────────────────────────────────────────────────╯
now(function()
    if vim.g.neovide then
        vim.g.neovide_scroll_animation_length = 0.1
        vim.opt.mousescroll = "ver:10,hor:6"
        vim.opt.linespace = -1
        vim.g.neovide_theme = "dark"

        vim.g.neovide_floating_shadow = true
        vim.g.neovide_floating_z_height = 2
        vim.g.neovide_light_angle_degrees = 45
        vim.g.neovide_light_radius = 15

        vim.g.neovide_floating_blur_amount_x = 10.0
        vim.g.neovide_floating_blur_amount_y = 10.0

        vim.o.guicursor =
        "n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50,a:blinkwait100-blinkoff700-blinkon700-Cursor/lCursor,sm:block-blinkwait0-blinkoff300-blinkon300"
        vim.g.neovide_cursor_animation_length = 0.03
        vim.g.neovide_cursor_smooth_blink = true
        vim.g.neovide_cursor_vfx_mode = "pixiedust"
    end
end)

