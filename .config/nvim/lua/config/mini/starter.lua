Mvim_starter_custom = function()
    return {
        { name = "Recent Files", action = function() require("mini.extra").pickers.oldfiles() end, section = "Search" },
        { name = "Session",      action = function() require("mini.sessions").select() end,        section = "Search" },
    }
end
require("mini.starter").setup({
    autoopen = true,
    items = {
        -- require("mini.starter").sections.builtin_actions(),
        Mvim_starter_custom(),
        require("mini.starter").sections.recent_files(5, false, false),
        require("mini.starter").sections.recent_files(5, true, false),
        require("mini.starter").sections.sessions(5, true),
    },
    header = [[
      ___                                    ___     
     /  /\          ___        ___          /__/\    
    /  /::\        /__/\      /  /\        |  |::\   
   /  /:/\:\       \  \:\    /  /:/        |  |:|:\  
  /  /:/  \:\       \  \:\  /__/::\      __|__|:|\:\ 
 /__/:/ \__\:\  ___  \__\:\ \__\/\:\__  /__/::::| \:\
 \  \:\ /  /:/ /__/\ |  |:|    \  \:\/\ \  \:\~~\__\/
  \  \:\  /:/  \  \:\|  |:|     \__\::/  \  \:\      
   \  \:\/:/    \  \:\__|:|     /__/:/    \  \:\     
    \  \::/      \__\::::/      \__\/      \  \:\    
     \__\/           ~~~~                   \__\/    
    ]]
})
