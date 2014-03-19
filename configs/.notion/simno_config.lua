defbindings("WScreen", {
    bdoc("Forward-circulate focus."),
    kpress(META.."Left", "ioncore.goto_next(_chld, 'left')", 
           "_chld:non-nil"),
    kpress(META.."Right", "ioncore.goto_next(_chld, 'right')", 
           "_chld:non-nil"),
    -- Vertical window navigation
    kpress(META.."Down", "ioncore.goto_next(_chld, 'down')", 
           "_chld:non-nil"),
    kpress(META.."Up", "ioncore.goto_next(_chld, 'up')", 
           "_chld:non-nil"),
    submap(META.."K", { 
        bdoc("Backward-circulate focus."),
        kpress("AnyModifier+Shift+Tab", "ioncore.goto_prev(_chld, 'left')", 
               "_chld:non-nil")}),
})

defbindings("WFrame", {
    -- Horizontal window navigation
    kpress(META.."Right", "WFrame.switch_next(_)"),
    kpress(META.."Left", "WFrame.switch_prev(_)")
})
