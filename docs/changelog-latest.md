0.7.28.8
====

I hope you are having a nice Tuesday  
This update should fix many of the new audio issues caused by changes in 0.7.28.6

I have also adjusted input handling again slightly, it should be much more precise now  
Please complain promptly if you now have new performance issues, as CPU usage may be a bit higher

# Features
- Dragging and dropping .ruleset files onto the game is supported
- Dragging and dropping several files at a time onto the game is supported (max 20)
- Backgrounds can be deleted from charts from the right-click menu in level select
- Added a hotkey to toggle the Mirror mod (default Ctrl-M)
- After an import succeeds, the chart is automatically selected if you're on level select
- Interlude now keeps exact-original copies of charts it's converted, for useful future features

# Bug fixes
- Fixed an issue where audio offset starts way off and slowly drifts back in sync as you play
- Fixed preview not playing if the song has finished and you return to level select
- Changed the "offset drift" number in debug overlay to say 0.0 when no audio is playing
- Table download menus work when not logged in again

