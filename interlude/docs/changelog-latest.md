0.7.27.11
====

Something new

The difficulty calculator and performance rating has been overhauled for the first time in about 5 or 6 years  
Go nuts basically (and tell me your thoughts/feedback)  

This will not be the only overhaul in the coming months  
The system is subject to many changes (such as LN rating in the near future) so your opinions Will shape how it ultimately turns out for the 1.0 release

# New features
- Borderless fullscreen mode is back
- You can navigate to the next/previous chart while the preview is open
- KPS meter can now show max, average and total KPS too
- You can change local offset from the score screen
- You can flip all HUD positionings (or for individual elements) horizontally and vertically in the editor
- Group by Difficulty
- Filter charts by creator by typing `mapper=...` in the search bar
- Shift+D switches the ruleset to the "original" for the chart on score screen (requires reimporting charts before it will work)
- For those interested, press F2 on the chart preview for difficulty rating debug tools

# Improvements
- Buttons to directly edit HUD/Noteskin from options menu
- Preview in gameplay options is lower to not obscure other settings in 4:3
- Slight adjustment to appearance of level select
- Etterna metadata is preferred over osu! metadata, rather than taking latest import if you have both versions
- What was 50% volume before is now 100% volume since max volume before was deafening and never getting used
- Receptor offset slider now goes to +-1000%
- Mods are now abbreviated on level select, scores and the score screen

# Bug fixes
- Fixed a little bit of audio sometimes playing when pressing "continue" on score screen
- Fixed notes not getting the right snap colors in long charts
- Fixed some issues with importing replays from uprated osu! versions of charts
- Fixed a long-lasting bug since 7th October 2024, osu! charts imported from before then can have a different ID to after
  Reimporting your osu! charts will move all scores onto the single correct ID for the chart (and can bring back scores that have gone missing)

