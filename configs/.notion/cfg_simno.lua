defbindings("WScreen", {
    bdoc("Forward-circulate display focus."),
    kpress(META.."Tab", "ioncore.goto_next(_chld, 'left')", "_chld:non-nil"),

    bdoc("Backward-circulate display focus."),
    kpress(META.."Shift+Tab", "ioncore.goto_next(_chld, 'right')", "_chld:non-nil"),

    bdoc("Switch to frame above"),
    kpress(META.."Up", "ioncore.goto_next(_chld, 'up')", "_chld:non-nil"),

    bdoc("Switch to frame below"),
    kpress(META.."Down", "ioncore.goto_next(_chld, 'down')", "_chld:non-nil"),
})

defbindings("WFrame", {
    bdoc("Switch to frame on the right"),
    kpress(META.."Right", "WFrame.switch_next(_)"),

    bdoc("Switch to frame on the left"),
    kpress(META.."Left", "WFrame.switch_prev(_)")
})
