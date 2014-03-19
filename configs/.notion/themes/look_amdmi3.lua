-- look_brownsteel.lua drawing engine configuration file for Ion.

if not gr.select_engine("de") then return end

de.reset()

de.defstyle("*", {
    shadow_colour = "#404040",
    highlight_colour = "#707070",
    background_colour = "#505050",
    foreground_colour = "#a0a0a0",
    padding_pixels = 0,
    highlight_pixels = 1,
    shadow_pixels = 1,
    border_style = "elevated",
    font = "-*-helvetica-medium-r-normal-*-14-*-*-*-*-*-koi8-r",
    text_align = "center",
})

de.defstyle("stdisp-dock", {
    based_on = "*",
    background_colour = "#000000",
    outline_style = "none",
})
--[[
de.defstyle("frame", {
    based_on = "*",
    shadow_colour = "#404040",
    highlight_colour = "#707070",
    padding_colour = "#505050",
    background_colour = "#000000",
    foreground_colour = "#ffffff",
    padding_pixels = 0,
    highlight_pixels = 0,
    shadow_pixels = 0,
})
]]--

de.defstyle("frame", {
    based_on = "*",
    shadow_colour = "#000000",
    highlight_colour = "#000000",
    padding_colour = "#000000",
    background_colour = "#000000",
    foreground_colour = "#000000",
    padding_pixels = 0,
    highlight_pixels = 1,
    shadow_pixels = 1,
})

de.defstyle("stdisp-statusbar", {
    based_on = "*",
    highlight_pixels = 1,
    shadow_pixels = 1,
    border_style = "elevated",
})

de.defstyle("frame-tiled", {
    based_on = "frame",
    border_style = "inlaid",
    spacing = 0,
})

de.defstyle("frame-scratchpad", {
    based_on = "frame",
    border_style = "inlaid",
    spacing = 0,
    padding_pixels = 0,
    highlight_pixels = 1,
    shadow_pixels = 1,
})

de.defstyle("frame-floating", {
    based_on = "frame",
    border_style = "ridge",
})

de.defstyle("tab", {
    based_on = "*",
    --font = "-*-helvetica-medium-r-normal-*-12-*-*-*-*-*-koi8-r",
    font = "-*-helvetica-medium-r-normal-*-12-*-*-*-*-*-iso10646-1",
    --font = "-*-terminus-medium-r-normal-*-14-*-*-*-*-*-iso10646-1",
    de.substyle("active-selected", {
        shadow_colour = "#304050",
        highlight_colour = "#708090",
        background_colour = "#506070",
        foreground_colour = "#ffffff",
    }),
    de.substyle("active-unselected", {
        shadow_colour = "#203040",
        highlight_colour = "#607080",
        background_colour = "#405060",
        foreground_colour = "#a0a0a0",
    }),
    de.substyle("inactive-selected", {
        shadow_colour = "#404040",
        highlight_colour = "#909090",
        background_colour = "#606060",
        foreground_colour = "#a0a0a0",
    }),
    de.substyle("inactive-unselected", {
        shadow_colour = "#404040",
        highlight_colour = "#707070",
        background_colour = "#505050",
        foreground_colour = "#a0a0a0",
    }),
    text_align = "center",
})

de.defstyle("tab-frame", {
    based_on = "tab",
    de.substyle("*-*-*-*-activity", {
        shadow_colour = "#401010",
        highlight_colour = "#907070",
        background_colour = "#990000",
        foreground_colour = "#eeeeee",
    }),
})

de.defstyle("tab-frame-tiled", {
    based_on = "tab-frame",
    spacing = 0,
})

de.defstyle("tab-menuentry", {
    based_on = "tab",
    text_align = "left",
    de.substyle("active-selected", {
        shadow_colour = "#506070",
        highlight_colour = "#506070",
        background_colour = "#506070",
        foreground_colour = "#ffffff",
    }),
    de.substyle("active-unselected", {
        shadow_colour = "#405060",
        highlight_colour = "#405060",
        background_colour = "#405060",
        foreground_colour = "#a0a0a0",
    }),
    de.substyle("inactive-selected", {
        shadow_colour = "#606060",
        highlight_colour = "#606060",
        background_colour = "#606060",
        foreground_colour = "#a0a0a0",
    }),
    de.substyle("inactive-unselected", {
        shadow_colour = "#505050",
        highlight_colour = "#505050",
        background_colour = "#505050",
        foreground_colour = "#a0a0a0",
    }),
    highlight_pixels = 0,
    shadow_pixels = 0,
    padding_pixels = 2,
})

de.defstyle("tab-menuentry-big", {
    based_on = "tab-menuentry",
    font = "-*-helvetica-medium-r-normal-*-12-*-*-*-*-*-koi8-r",
    padding_pixels = 5,
})

de.defstyle("input", {
    based_on = "*",
    shadow_colour = "#404040",
    highlight_colour = "#707070",
    background_colour = "#000000",
    foreground_colour = "#ffffff",
    padding_pixels = 1,
    highlight_pixels = 1,
    shadow_pixels = 1,
    border_style = "elevated",
    de.substyle("*-cursor", {
        background_colour = "#ffffff",
        foreground_colour = "#000000",
    }),
    de.substyle("*-selection", {
        background_colour = "#505050",
        foreground_colour = "#ffffff",
    }),
})

de.defstyle("input-menu", {
    based_on = "*",
    de.substyle("active", {
        shadow_colour = "#304050",
        highlight_colour = "#708090",
        background_colour = "#506070",
        foreground_colour = "#ffffff",
    }),
})

--dopath("lookcommon_emboss")

gr.refresh()

