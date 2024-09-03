0.7.26.1
====

Bug fix update (and some UI changes)

# Bug fixes
- Fixed a crash when spectating in multiplayer
- Fixed a crash when someone sends invalid replay data during multiplayer
- Fixed pasting an image from the clipboard breaking your clipboard until the game is closed thanks to @9382
- Fixed a pixel seam when maximising preview in options menus
- Fixed a performance issue in level select
- Fixed ruleset recalculation using scores with mods that unrank the score

# New UI stuff
- Hotkey to reset local offset when changing it
- More stats can be shown on score screen by clicking on MA/PA stat line
- Appearance of graph hover info improved thanks to @Lylcaruis1
- Hover over lamp on score screen to see "raw mistakes" experimental feature
- Practice mode UI has been overhauled
- Some adjustments to level select UI

# Rewrites
- Part of keyboard input handling has been rewritten, which allows the game to listen to key "repeats" properly
- Input latency may be slightly lower now due to the code doing less but I haven't measured it
- osu! beatmap and skin.ini parser has been rewritten

