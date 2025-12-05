return {
    -- Statusbar
    {'vimpostor/vim-tpipeline'},
    -- Themes
    {"catppuccin/nvim", name = "catppuccin", priority = 1000},
    {
        "rose-pine/neovim",
        name = "rose-pine",
        config = function()
            vim.cmd("colorscheme rose-pine")
        end
    },
    {"theacodes/witchhazel"}, -- Utility
    {
        "swaits/zellij-nav.nvim",
        lazy = true,
        event = "VeryLazy",
        keys = {
            {
                "<c-h>", "<cmd>ZellijNavigateLeftTab<cr>",
                {silent = true, desc = "navigate left or tab"}
            }, {
                "<c-j>", "<cmd>ZellijNavigateDown<cr>",
                {silent = true, desc = "navigate down"}
            },
            {
                "<c-k>", "<cmd>ZellijNavigateUp<cr>",
                {silent = true, desc = "navigate up"}
            }, {
                "<c-l>", "<cmd>ZellijNavigateRightTab<cr>",
                {silent = true, desc = "navigate right or tab"}
            }
        },
        opts = {}
    }, {
        "folke/flash.nvim",
        event = "VeryLazy",
        ---@type Flash.Config
        opts = {},
        keys = {
            {
                "s",
                mode = {"n", "x", "o"},
                function() require("flash").jump() end,
                desc = "Flash"
            }, {
                "S",
                mode = {"n", "x", "o"},
                function() require("flash").treesitter() end,
                desc = "Flash Treesitter"
            }, {
                "r",
                mode = "o",
                function() require("flash").remote() end,
                desc = "Remote Flash"
            }, {
                "R",
                mode = {"o", "x"},
                function() require("flash").treesitter_search() end,
                desc = "Treesitter Search"
            }, {
                "<c-s>",
                mode = {"c"},
                function() require("flash").toggle() end,
                desc = "Toggle Flash Search"
            }
        }
    }, {
        'williamboman/mason.nvim',
        opts = {
            ui = {
                icons = {
                    package_installed = '✓',
                    package_pending = '➜',
                    package_uninstalled = '✗'
                }
            }
        }
    }, {
        'nvim-telescope/telescope.nvim',
        tag = '0.1.8',
        dependencies = {'nvim-lua/plenary.nvim'},
        defaults = {
            layout_config = {
                vertical = {width = 0.8},
                horizontal = {width = 0.8}
            }
        },
        opts = {
            pickers = {
                find_files = {theme = "dropdown"},
                live_grep = {theme = "dropdown"},
                buffers = {theme = "dropdown"},
                help_tags = {theme = "dropdown"}
            }
        }
    }, -- Treesitter
    {"nvim-treesitter/nvim-treesitter", build = ":TSUpdate"}, -- Neotree
    {
        "nvim-neo-tree/neo-tree.nvim",
        branch = "v3.x",
        dependencies = {
            "nvim-lua/plenary.nvim", "nvim-tree/nvim-web-devicons",
            "MunifTanjim/nui.nvim"
        },
        lazy = false,
        ---@module "neo-tree"
        ---@type neotree.Config?
        opts = {
            -- fill any relevant options here
        }
    }, -- Mini Nvim
    {'echasnovski/mini.nvim', version = '*'}, -- LSP Config
    {
        "neovim/nvim-lspconfig",
        dependencies = {'saghen/blink.cmp'},
        config = function()
            vim.lsp.config("*", {})
            vim.lsp.enable({
                "gopls", "jdtls", "kotlin_language_server", "lua_ls", "pylsp",
                "rust_analyzer", "ts_ls", "ols", "clangd"
            })
        end
    }, -- Blink CMP
    {
        'saghen/blink.cmp',
        dependencies = {'rafamadriz/friendly-snippets'},

        -- use a release tag to download pre-built binaries
        version = '1.*',

        ---@module 'blink.cmp'
        ---@type blink.cmp.Config
        opts = {
            keymap = {preset = 'default'},

            appearance = {nerd_font_variant = 'mono'},

            completion = {documentation = {auto_show = false}},

            -- Default list of enabled providers defined so that you can extend it
            -- elsewhere in your config, without redefining it, due to `opts_extend`
            sources = {default = {'lsp', 'path', 'snippets', 'buffer'}},
            -- (Default) Rust fuzzy matcher for typo resistance and significantly better performance
            -- You may use a lua implementation instead by using `implementation = "lua"` or fallback to the lua implementation,
            -- when the Rust fuzzy matcher is not available, by using `implementation = "prefer_rust"`
            --
            -- See the fuzzy documentation for more information
            fuzzy = {implementation = "prefer_rust_with_warning"}
        },
        opts_extend = {"sources.default"}
    }, -- Popups
    {
        "folke/noice.nvim",
        event = "VeryLazy",
        opts = {
            lsp = {
                -- override markdown rendering so that **cmp** and other plugins use **Treesitter**
                override = {
                    ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
                    ["vim.lsp.util.stylize_markdown"] = true,
                    ["cmp.entry.get_documentation"] = true -- requires hrsh7th/nvim-cmp
                }
            },
            presets = {
                bottom_search = false, -- use a classic bottom cmdline for search
                command_palette = true, -- position the cmdline and popupmenu together
                long_message_to_split = true, -- long messages will be sent to a split
                lsp_doc_border = true -- add a border to hover docs and signature help
            }
        },
        dependencies = {
            -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
            "MunifTanjim/nui.nvim"
        }
    }, -- C/C++ clang-format
    {"nvimdev/guard.nvim", dependencies = {"nvimdev/guard-collection"}},
    {'Civitasv/cmake-tools.nvim', dependencies = {'stevearc/overseer.nvim'}},
    {'akinsho/toggleterm.nvim', version = "*", config = true},
    {'mfussenegger/nvim-dap'}, {
        "rcarriga/nvim-dap-ui",
        dependencies = {"mfussenegger/nvim-dap", "nvim-neotest/nvim-nio"}
    }
}
