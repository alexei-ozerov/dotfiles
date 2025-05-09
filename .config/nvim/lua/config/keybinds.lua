-- ╔═══════════════════════╗
-- ║    Local Variables    ║
-- ╚═══════════════════════╝
local keymap = vim.keymap.set

local split_sensibly = function()
    if vim.api.nvim_win_get_width(0) > math.floor(vim.api.nvim_win_get_height(0) * 2.3) then
        vim.cmd("vs")
    else
        vim.cmd("split")
    end
end

-- ╔═══════════════════════╗
-- ║    General Keymaps    ║
-- ╚═══════════════════════╝
keymap("n", "<leader>q", "<cmd>wqa<cr>", { desc = 'Quit' })
keymap("n", "ö", ":")
keymap("i", "<C-S-v>", "<C-r><C-o>*", { desc = 'Paste from System in Insertmode' })
keymap("n", "<S-Insert>", "p", { desc = 'Remap Paste for CopyQ' })
keymap("i", "<S-Insert>", "<C-R>+", { desc = 'Remap Paste for CopyQ' })

-- ╔══════════════════════╗
-- ║    Buffer Keymaps    ║
-- ╚══════════════════════╝
keymap("n", "<leader>bd", "<cmd>bd<cr>", { desc = 'Close Buffer' })
keymap("n", "<leader>bq", "<cmd>%bd|e#<cr>", { desc = 'Close other Buffers' })
keymap("n", "<S-l>", "<cmd>bnext<cr>", { desc = 'Next Buffer' })
keymap("n", "<S-h>", "<cmd>bprevious<cr>", { desc = 'Previous Buffer' })
keymap("n", "<TAB>", "<C-^>", { desc = "Alternate buffers" })
-- Format Buffer
-- With and without LSP
if vim.tbl_isempty(vim.lsp.get_clients()) then
    keymap("n", "<leader>bf", function() vim.lsp.buf.format() end,
        { desc = 'Format Buffer' })
else
    keymap("n", "<leader>bf", "gg=G<C-o>", { desc = 'Format Buffer' })
end

-- ╔═══════════════════╗
-- ║    LSP Keymaps    ║
-- ╚═══════════════════╝
keymap("n", "<leader>ld", function() vim.lsp.buf.definition() end,
    { desc = 'Go To Definition' })
keymap("n", "<leader>ls", "<cmd>Pick lsp scope='document_symbol'<cr>",
    { desc = 'Show all Symbols' })
keymap("n", "<leader>lr", function() vim.lsp.buf.rename() end, { desc = 'Rename This' })
keymap("n", "<leader>la", function() vim.lsp.buf.code_action() end,
    { desc = 'Code Actions' })
keymap("n", "<leader>le", function() require('mini.extra').pickers.diagnostic({ scope = "current" }) end,
    { desc = "LSP Errors in Buffer" })
keymap("n", "<leader>lf", function()
    vim.diagnostic.setqflist({ open = true })
end, { desc = "LSP Quickfix" })

-- ╔══════════════════╗
-- ║    UI Keymaps    ║
-- ╚══════════════════╝
-- Window Navigation
keymap("n", "<C-l>", "<cmd>wincmd l<cr>", { desc = 'Focus Right' })
keymap("n", "<C-k>", "<cmd>wincmd k<cr>", { desc = 'Focus Up' })
keymap("n", "<C-j>", "<cmd>wincmd j<cr>", { desc = 'Focus Down' })
keymap("n", "<C-h>", "<cmd>wincmd h<cr>", { desc = 'Focus Left' })

keymap("n", "<leader>wq", "<cmd>wincmd q<cr>", { desc = 'Close Window' })
keymap("n", "<leader>n", "<cmd>noh<cr>", { desc = 'Clear Search Highlight' })

--  ─( Split "Sensibly" )───────────────────────────────────────────────
-- Should automatically split or vsplit based on Ratios
keymap("n", "<leader>bs", split_sensibly, { desc = "Alternate buffers" })

--  ─( Neotree )────────────────────────────────────────────────────────
keymap("n", "<leader>tt", "<cmd>Neotree toggle<cr>", { desc = 'Neotree' })
-- keymap("n", "<leader>tt", function()
--     local reveal_file = vim.fn.expand('%:p')
--     if (reveal_file == '') then
--         reveal_file = vim.fn.getcwd()
--     else
--         local f = io.open(reveal_file, "r")
--         if (f) then
--             f.close(f)
--         else
--             reveal_file = vim.fn.getcwd()
--         end
--     end
--     require("neo-tree.command").execute({
--         action = "focus",
--         source = "filesystel",
--         position = "left",
--         reveal_file = reveal_file,
--         reveal_force_cwd = true,
--     })
-- end, { desc = 'Neotree' })
--
--  ─( Trying a "Center Code" Keymap )──────────────────────────────────
keymap("n", "<leader>uc", function()
    local margin = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_open_win(margin, false, {
        split = 'left',
        win = 0,
        style = 'minimal',
        width = 40
    })
end, { desc = 'Center Buffer' })
