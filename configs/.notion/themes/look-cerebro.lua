-- look-cerebro was changed from look-dusky.lua by daelstorm

if not gr.select_engine("de") then return end

de.reset()

de.defstyle("*", {
    shadow_colour = "#404040",
    highlight_colour = "#707070",
    background_colour = "#204444",
    foreground_colour = "#a0a0a0",
    padding_pixels = 1,
    highlight_pixels = 1,
    shadow_pixels = 1,
    border_style = "elevated",
    font = "-*-helvetica-medium-r-normal-*-14-*-*-*-*-*-*-*",
    text_align = "center",
})

de.defstyle("frame", {
    based_on = "*",
    shadow_colour = "#001010",
    highlight_colour = "#123636",
    padding_colour = "#204444",
    background_colour = "#000000",
    foreground_colour = "#ffffff",
    padding_pixels = 2,
    highlight_pixels = 1,
    shadow_pixels = 1,
    de.substyle("active", {
        shadow_colour = "#273f45",
        highlight_colour = "#668d92",
        padding_colour = "#66b5b8",
        foreground_colour = "#ffffff",
    }),
    transparent_background = true,
})

de.defstyle("frame-ionframe", {
    based_on = "frame",
    border_style = "inlaid",
    padding_pixels = 1,
    spacing = 1,
})

de.defstyle("frame-floatframe", {
    based_on = "frame",
    border_style = "ridge"
})

de.defstyle("tab", {
    based_on = "*",
    font = "-*-helvetica-medium-r-normal-*-12-*-*-*-*-*-*-*",
    de.substyle("active-selected", {
        shadow_colour = "#273f45",
        highlight_colour = "#668d92",
        background_colour = "#66b5b8",
        foreground_colour = "#000000",
    }),
    de.substyle("active-unselected", {
        shadow_colour = "#183335",
        highlight_colour = "#587676",
        background_colour = "#174040",
        foreground_colour = "#90cce0",
    }),
    de.substyle("inactive-selected", {
        shadow_colour = "#000f12",
        highlight_colour = "#356a75",
        background_colour = "#003131",
        foreground_colour = "#90cce0",
    }),
    de.substyle("inactive-unselected", {
        shadow_colour = "#002222",
        highlight_colour = "#356a75",
        background_colour = "#234545",
        foreground_colour = "#a0a0a0",
    }),
    text_align = "center",
})

de.defstyle("tab-frame", {
    based_on = "tab",
    de.substyle("*-*-*-*-activity", {
        shadow_colour = "#404040",
        highlight_colour = "#707070",
        background_colour = "#990000",
        foreground_colour = "#eeeeee",
    }),
})

de.defstyle("tab-frame-ionframe", {
    based_on = "tab-frame",
    spacing = 1,
})

de.defstyle("tab-menuentry", {
    based_on = "tab",
    text_align = "left",
    highlight_pixels = 0,
    shadow_pixels = 0,
})

de.defstyle("tab-menuentry-big", {
    based_on = "tab-menuentry",
    font = "-*-helvetica-medium-r-normal-*-17-*-*-*-*-*-*-*",
    padding_pixels = 7,
})

de.defstyle("input", {
    based_on = "*",
    shadow_colour = "#404040",
    highlight_colour = "#707070",
    background_colour = "#000000",
    foreground_colour = "#ffffff",
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
        shadow_colour = "#273f45",
        highlight_colour = "#668d92",
        background_colour = "#66b5b8",
        foreground_colour = "#ffffff",
    }),
})

gr.refresh()

