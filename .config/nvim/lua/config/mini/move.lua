require('mini.move').setup({
	  -- Module mappings. Use `''` (empty string) to disable one.
	  mappings = {
	    -- Move visual selection in Visual mode. Defaults are Alt (Meta) + hjkl.
	    left = '<S-h>',
	    right = '<S-l>',
	    down = '<S-j>',
	    up = '<S-k>',

	    -- Move current line in Normal mode
	    line_left = '<M-S-h>',
	    line_right = '<M-S-l>',
	    line_down = '<M-S-j>',
	    line_up = '<M-S-k>',
	  },

	  -- Options which control moving behavior
	  options = {
	    -- Automatically reindent selection during linewise vertical move
	    reindent_linewise = true,
	  },
})
