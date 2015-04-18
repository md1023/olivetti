--
-- Notion main configuration file
--
-- This file only includes some settings that are rather frequently altered.
-- The rest of the settings are in cfg_notioncore.lua and individual modules'
-- configuration files (cfg_modulename.lua). 

META="Mod4+"
--ALTMETA=""

-- Terminal emulator
if os and os.execute("test -x /usr/bin/terminator") == 0 then
    XTERM="/usr/bin/terminator"
else
    XTERM="/usr/bin/xterm"
end

-- File manager
if os and os.execute("test -x /usr/bin/terminator") then
    FILEMANAGER="/usr/bin/dolphin"
end

-- Browser
if os and os.execute("test -x /usr/bin/seamonkey") then
    BROWSER="/usr/bin/seamonkey"
end

-- Emacs
if os and os.execute("test -x /usr/bin/emacs") then
    EMACSCLIENT="/usr/bin/emacsclient -c"
end

-- Program launcher
if os and os.execute("test -x /usr/bin/gmrun") == 0 then
    LAUNCHER="ioncore.exec_on(_, '/usr/bin/gmrun')"
else
    LAUNCHER="mod_query.query_exec(_)"
end

-- Some basic settings
ioncore.set{
    -- Maximum delay between clicks in milliseconds to be considered a
    -- double click.
    dblclick_delay=250,

    opaque_resize=true,

    -- Don't move the mouse cursor when changing frames with the keyboard
    warp=true,

    -- Default index for windows in frames: one of 'last', 'next' (for
    -- after current), or 'next-act' (for after current and anything with
    -- activity right after it).
    frame_default_index='next-act',
    
    -- Auto-unsqueeze transients/menus/queries.
    unsqueeze=true,
    
    -- Display notification tooltips for activity on hidden workspace.
    -- This is awesome in conjunction with urgentOnBell and a bell in the PROMPT
    screen_notify=true,
}

-- dopath("cfg_defaults")
--
-- Notion default settings
--

dopath("cfg_notioncore")
dopath("cfg_simno")
dopath("cfg_mouse")
dopath("cfg_menuitems")
dopath("cfg_kludges")
dopath("cfg_layouts")

if os and os.execute("test -r ~/.notion/cfg_local.lua") == 0 then
    dopath("cfg_local") -- Anything that you don't want to share across machines
end

dopath("mod_query")
dopath("mod_menu")
dopath("mod_tiling")
dopath("mod_sp")
-- end default setting

dopath("look_simno")
