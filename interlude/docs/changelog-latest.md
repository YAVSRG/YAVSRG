0.7.28.1
====

Hello I am back from a brief hiatus - combo of life events, spending long on some unreleased features that will be released soon (tm) and also just taking a bit of time off from the project
Please accept this humble bug fix update and likely quite a bulky feature update in a couple of business weeks

# New features
- Option for the Input Meter HUD element to show your keybinds by @Lylcaruis
  Many keybinds now also show as just the symbol on the key
- Ctrl-Shift-R now works from inside many menus to reload the current thing you're editing
- Added a comparison screen so you can see how one ruleset compares to another
- Support for Quaver 3-10K charts instead of just 4 and 7

# Improvements
- Volume slider icon is slightly more responsive
- Color pickers now have a big square previewing the color
- Playtime stats are shown in hours only and not days
- Some menu layout improvements on 4:3 ratio
- You now have to click off an element in the HUD editor to deselect it, making it harder to accidentally select something else when you meant to drag
- Lots of little UI tweaks
- You can now change the osu! linked path even if an incorrect path was auto-detected

# Bug fixes
- Fixed an error when the game restarts after updating
- Fixed a crash that can happen right at the start of playing a song
- Fixed level select wheel not scrolling when you change songs with the preview open
- Fixed LN< and LN> level select filters being backwards
- Fixed a crash when a ruleset has no grades
- Fixed being able to turn on Column Swap alongside incompatible mods
- Fixed osu! rulesets having a `,` in the name depending on system locale
- Not a full fix but if your options.json is corrupt the game automatically uses the backup for you

