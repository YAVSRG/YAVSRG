0.7.15
====

Feature update 1 of a few in a short span of time

# Bug fixes
- You can no longer switch presets or reload noteskins while playing
- Note colors now update correctly when swapping presets outside the options menu
- Newly installed tables stay "selected" instead of requiring a manual re-select upon game restart

# Improvements
- LN percentage indicator on level select switches to a plain count of LNs if they make up <0.5% of the chart, requested by Jup

# New features
- You can set a mode on your presets so when it is selected, all changes are automatically saved to it and you don't need to manually save before switching to another preset
- You can set your presets to automatically switch depending on the keymode of the chart you are looking at in level select
  Example use: To use a different scroll speed, hit position and noteskin for 4k and 7k
  
How the render engine loads textures has been improved to make this possible, swapping noteskins should be seamless due to preloading the noteskins used by your presets

This also lifts the technical limitations on a couple of other features coming soon :)

Because some parts of the engine have changed, please report any unusual bugs in the Discord - Everything should look and perform exactly the same as before (or marginally better) but keep an eye out

