return {
    -- Theme
    { "theacodes/witchhazel" },
    {
      'uZer/pywal16.nvim',
      config = function()
        vim.cmd.colorscheme("pywal16")
      end,
  },
  {
      "swaits/zellij-nav.nvim",
      lazy = true,
      event = "VeryLazy",
      keys = {
        { "<c-h>", "<cmd>ZellijNavigateLeftTab<cr>",  { silent = true, desc = "navigate left or tab"  } },
        { "<c-j>", "<cmd>ZellijNavigateDown<cr>",  { silent = true, desc = "navigate down"  } },
        { "<c-k>", "<cmd>ZellijNavigateUp<cr>",    { silent = true, desc = "navigate up"    } },
        { "<c-l>", "<cmd>ZellijNavigateRightTab<cr>", { silent = true, desc = "navigate right or tab" } },
      },
      opts = {},
    },
    {
        'williamboman/mason.nvim',
        opts = {
            ui = {
                icons = {
                    package_installed = '✓',
                    package_pending = '➜',
                    package_uninstalled = '✗',
                },
            },
        },
    },
    {
      "nvim-java/nvim-java",
      config = false,
      dependencies = {
        {
          "neovim/nvim-lspconfig",
          opts = {
            servers = {
              jdtls = {
                settings = {
                  java = {
                    configuration = {
                      runtimes = {
                        {
                          name = "Java17",
                          path = "/home/aozerov/builds/java-17-openjdk-17.0.11.0.9-1.portable.jdk.el.x86_64",
                        },
                        {
                          name = "Java25",
                          path = "/home/aozerov/builds/jdk-25",
                        },
                      },
                    },
                  },
                },
              },
            },
            setup = {
              jdtls = function()
                require("java").setup({
                  -- root_markers = {
                  --   "settings.gradle",
                  --   "settings.gradle.kts",
                  --   "pom.xml",
                  --   "build.gradle",
                  --   "mvnw",
                  --   "gradlew",
                  --   "build.gradle",
                  --   "build.gradle.kts",
                  -- },
                })
              end,
            },
          },
        },
      },
    },
    {
        "EdenEast/nightfox.nvim",
        modules = { 'neotree' },
        opts = {
          transparent = true,
          styles = {
             sidebars = "transparent",
             floats = "transparent",
          },
       },
    },
    -- Treesitter
    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate"
    },
    -- Neotree
    {
        "nvim-neo-tree/neo-tree.nvim",
        branch = "v3.x",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "nvim-tree/nvim-web-devicons",
            "MunifTanjim/nui.nvim",
        },
        lazy = false,
        ---@module "neo-tree"
        ---@type neotree.Config?
        opts = {
            -- fill any relevant options here
        },
    },
    {
        'nvim-telescope/telescope.nvim', tag = '0.1.8',
    },
    -- Mini Nvim
    {
        'echasnovski/mini.nvim',
        version = '*',
    },
    -- LSP Config
    {
        "neovim/nvim-lspconfig",
        dependencies = { 'saghen/blink.cmp' },
            config = function()
                vim.lsp.config("*", {})
                vim.lsp.enable({
                    "gopls",
                    "jdtls",
                    "kotlin_language_server",
                    "lua_ls",
                    "pylsp",
                    "rust_analyzer",
                    "ts_ls",
                })
            end,
    },
    {
        'nvim-java/nvim-java'
    },
    -- Blink CMP
    {
        'saghen/blink.cmp',
        dependencies = { 'rafamadriz/friendly-snippets' },

        -- use a release tag to download pre-built binaries
        version = '1.*',

        ---@module 'blink.cmp'
        ---@type blink.cmp.Config
        opts = {
            -- 'default' (recommended) for mappings similar to built-in completions (C-y to accept)
            -- 'super-tab' for mappings similar to vscode (tab to accept)
            -- 'enter' for enter to accept
            -- 'none' for no mappings
            --
            -- All presets have the following mappings:
            -- C-space: Open menu or open docs if already open
            -- C-n/C-p or Up/Down: Select next/previous item
            -- C-e: Hide menu
            -- C-k: Toggle signature help (if signature.enabled = true)
            --
            -- See :h blink-cmp-config-keymap for defining your own keymap
            keymap = {
                preset = 'default',
            },

            appearance = {
                -- 'mono' (default) for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
                -- Adjusts spacing to ensure icons are aligned
                nerd_font_variant = 'mono'
            },

            -- (Default) Only show the documentation popup when manually triggered
            completion = { documentation = { auto_show = false } },

            -- Default list of enabled providers defined so that you can extend it
            -- elsewhere in your config, without redefining it, due to `opts_extend`
            sources = {
                default = { 'lsp', 'path', 'snippets', 'buffer' },
            },

            -- (Default) Rust fuzzy matcher for typo resistance and significantly better performance
            -- You may use a lua implementation instead by using `implementation = "lua"` or fallback to the lua implementation,
            -- when the Rust fuzzy matcher is not available, by using `implementation = "prefer_rust"`
            --
            -- See the fuzzy documentation for more information
            fuzzy = { implementation = "prefer_rust_with_warning" }
        },
        opts_extend = { "sources.default" }
    },
    -- Popups
    {
        "folke/noice.nvim",
        event = "VeryLazy",
        opts = {
            lsp = {
                -- override markdown rendering so that **cmp** and other plugins use **Treesitter**
                override = {
                  ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
                  ["vim.lsp.util.stylize_markdown"] = true,
                  ["cmp.entry.get_documentation"] = true, -- requires hrsh7th/nvim-cmp
                },
              },
              presets = {
                bottom_search = false, -- use a classic bottom cmdline for search
                command_palette = true, -- position the cmdline and popupmenu together
                long_message_to_split = true, -- long messages will be sent to a split
                lsp_doc_border = false, -- add a border to hover docs and signature help
              },
        },
        dependencies = {
            -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
            "MunifTanjim/nui.nvim",
        }
    }
}
