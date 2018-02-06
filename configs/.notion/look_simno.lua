-- look-awesome-sm.lua drawing engine configuration file for Ion.

--gr.select_engine("xftde")
if not gr.select_engine("de") then return end

de.reset()

de.defstyle("*", {
    shadow_colour = "#99A",
    highlight_colour = "#99A",
    background_colour = "#667",
    foreground_colour = "#FFF",
    padding_colour = "#99A",

    border_style = "elevated",

    highlight_pixels = 0,
    shadow_pixels = 0,
    padding_pixels = 0,
    spacing = 0,
    font = "-*-*-*-r-normal-*-13-*-*-*-*-*-koi8-*",
    text_align = "center",
})

de.defstyle("frame", {
    based_on = "*",
    background_colour = "black",
    transparent_background = false,
})

de.defstyle("statusbar", {
    based_on = "*",
--    foreground_colour = "#dcdccc",
--    background_colour = "#595959",
    foreground_colour = "#999",
    background_colour = "#334",
    highlight_colour = "#667",
    shadow_colour = "#334",
    transparent_background = false,

    highlight_pixels = 1,
    spacing = 1,

    text_align = "center",
})

de.defstyle("frame-floatframe", {
    based_on = "frame",
    padding_pixels = 1,
    de.substyle("active", {
        padding_colour = "#99A",
    }),
    de.substyle("inactive", {
        padding_colour = "#666",
    }),
})

de.defstyle("tab", {
    based_on = "*",

    highlight_pixels = 1,
    shadow_pixels = 1,
    padding_pixels = 0,
    spacing = 1,

    transparent_background = false,

    text_align = "center",

    de.substyle("active-selected", {
--        shadow_colour = "#99A",
        shadow_colour = "#667",
        highlight_colour = "#99A",
--        background_colour = "#667",
        background_colour = "#595959",
--        foreground_colour = "#FFF",
        foreground_colour = "#dcdccc"
    }),
    de.substyle("active-unselected", {
--        shadow_colour = "#667",
        highlight_colour = "#667",
        background_colour = "#334",
        foreground_colour = "#999",
        shadow_colour = "#334",
    }),
    de.substyle("inactive-selected", {
--        shadow_colour = "#666",
        highlight_colour = "#666",
        background_colour = "#333",
        foreground_colour = "#888",
        shadow_colour = "#333",
    }),
    de.substyle("inactive-unselected", {
--        shadow_colour = "#333",
        highlight_colour = "#333",
        background_colour = "#111",
        foreground_colour = "#777",
        shadow_colour = "#111",
    }),
})

de.defstyle("tab-frame", {
    based_on = "tab",
    padding_pixels = 1,

    de.substyle("active-*-*-*-activity", {
        shadow_colour = "red",
        highlight_colour = "red",
    	background_colour = "#800",
        foreground_colour = "#FFF",
    }),
    de.substyle("inactive-*-*-*-activity", {
        shadow_colour = "#800",
        highlight_colour = "#800",
    	background_colour = "#400",
        foreground_colour = "#888",
    }),
})

de.defstyle("tab-frame-ionframe", {
    based_on = "tab-frame",
})

de.defstyle("tab-frame-floatframe", {
    based_on = "tab-frame",
    padding_pixels = 1,
})

de.defstyle("tab-menuentry", {
    based_on = "tab",

    padding_pixels = 3,
    spacing = 0,

    text_align = "left",
})

de.defstyle("tab-menuentry-bigmenu", {
    based_on = "tab-menuentry",

    padding_pixels = 7,
})
    
de.defstyle("tab-menuentry-pmenu", {
    based_on = "tab-menuentry",
    de.substyle("inactive-selected", {
        shadow_colour = "#99A",
        highlight_colour = "#99A",
        background_colour = "#667",
        foreground_colour = "#FFF",
    }),
    de.substyle("inactive-unselected", {
        shadow_colour = "#667",
        highlight_colour = "#667",
        background_colour = "#334",
        foreground_colour = "#999",
    }),
 })

de.defstyle("input", {
    based_on = "*",
    
    foreground_colour = "#FFF",
    background_colour = "#667",
    padding_colour = "#667",

    transparent_background = false,

    border_style = "elevated",

    padding_pixels = 2,
})

de.defstyle("input-edln", {
    based_on = "input",

    de.substyle("*-cursor", {
        background_colour = "#FFF",
        foreground_colour = "#667",
    }),
    de.substyle("*-selection", {
        background_colour = "#AAA",
        foreground_colour = "#334",
   }),
})

de.defstyle("input-message", {
    based_on = "input",
})

de.defstyle("input-menu", {
    based_on = "input",

    transparent_background = false,

    highlight_pixels = 0,
    shadow_pixels = 0,
    padding_pixels = 0,
    spacing = 0,
})

de.defstyle("input-menu-bigmenu", {
    based_on = "input-menu",
})

de.defstyle("moveres_display", {
    based_on = "input",
})

de.defstyle("dock", {
    based_on = "*",
})

gr.refresh()

