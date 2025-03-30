0.7.27.13
====

Hi it's me again

# Improvements
- Experimental change to the engine/frame cap, to see if it affects the Intel Crash of Death
- You can now view wider "slices" of the score graph when holding shift + hovering
- New option to show timestamps next to PBs on the song wheel (found in Level select options)
- You can now change the osu! hold mechanics to something else via the ruleset editor

Editing osu! scorev1 LN windows still can't be done ingame but this will help players who are trying to modify an osu! ruleset to emulate ScoreV2

0.7.27.12
====

Mostly bug fixes and changes to the codebase that won't matter to you as a player (but some of it might!)

# Improvements
- You can now hover over the imports button when tasks are running to see live progress bars
- The imports button itself also has a mini progress bar reflecting the current task
- Red warning shows if you are going to close the game while tasks are running
- The update button has a loading bar when downloading
- Exporting a .osz will now succeed even if the audio or background are missing
- Un-halved the volume slider after some players complained
- Some subtle things have been improved (symmetry, pixel accuracy) in many parts of the UI
- Charts that have assets 'linked' to external folders have a chain icon in level select
- Abbreviation for the Shuffle mod is now "SHF"

# Bug fixes
- Fixed an issue causing inputs to be more or less tied to framerate, affecting non-fullscreen users
- Fixed many minor issues with importing charts
- Fixed an issue where the wrong chart appears selected on level select
- Fixed skip button being deletable from your HUD, preventing you from skipping
- Fixed hitting 0 notes awarding NaN as the performance rating
- Fixed search box staying focused when opening preview
- Fixed chat box staying focused when entering multiplayer gameplay
- Fixed failing on scores with unranked mods always saving regardless of setting

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

0.7.27.10
====

This update changes many engine things, enjoy
Many more songs have now been given score leaderboards since last update (and more on the way)

# New features
- Game window opens faster, then has a loading screen while the rest of the game loads
- Audio engine should no longer have any offset drift at all across rates, in general offset will feel much more consistent now
- Screenshots automatically get copied to the clipboard when taken
- Screenshots do not include notifications/other screenshot popups in the image when taken
- CTRL+ALT+SHIFT+F1 crashes your game and opens the log, for easier bug reporting
- Song audio crossfades out/in when switching song instead of a hard cut
- Change column filters on score screen with number keys
- Timestamps are shown when hovering over the score graph
- Color pickers in all options menus are now an entire page instead of a small box

# Bug fixes
- Potential fix for various crashes and graphical glitches on Intel HD Graphics
- Fixed cloning rulesets and then editing the clone sometimes editing the original too
- Fixed bug with creating new lamps in rulesets that made the ordering unclear
- HUD editor now unpauses song if it was paused on entering
- Fixed issues with osu! Skin parsing when a decimal is used where an integer expected
- Song now correctly fades out during the transition into replay mode

0.7.27.9
====

This update adds leaderboards! Go check them out, let me know what you think in the usual places  
As well as all-time leaderboards there is a monthly leaderboard that resets at midnight UTC at the start of each month

# New features
- Leaderboards! They can now be viewed via the Stats menu ingame
- Some stats are now shown on user profiles ingame, where available

# Improvements
- Accuracy on score screen now reflects column filters if set
- Improved some tooltips and messages to make various features clearer (osu! replay loading, audio offset, etc)

# Bug fixes
- Fixed some audio engine issues where song audio wouldn't play or system-wide sound would mute
- Fixed multiple 'Default' audio devices appearing in the list
- Fixed a crash caused by unicode characters and Discord RPC
- Fixed API requests sporadically displaying 'Server error' ingame
- Fixed a crash when changing monitor settings if the monitor that was last selected is no longer available
- Fixed some issues with extracting noteskins

# Platform support
- Linux x64 is now officially supported by the auto-updater and as a download via the site
- Experimental builds for macOS, x64 architecture are now available for users who want to test them

0.7.27.8
====

A small bug fix update

# New features
- Letterboxing now supports offsets within the monitor in question
  This also applies to windowed mode, so you can have a default position other than the center of the monitor
- Your stats are now synced between your game client and the server
  This is not displayed anywhere yet, but enables various global leaderboards (probably next week)

# Improvements
- Skinning screen will scroll as you enter it so that the selected HUD and Noteskin are always visible
- Delete and context menu hotkeys behave more predictably when navigating scoreboard using keyboard
- Release error bars can be made smaller as well as larger

# Bug fixes
- Fixed some visual issues with the stats screen
- Fixed switching from fullscreen letterbox to fullscreen not working correctly
- Fixed presets not auto-switching if the first song you change to is a different keymode
- Fixed level select not returning to a playlist after continuing it in endless mode
- Fixed moving average weight slider clipping out of bounds
- Fixed option to delete a leaderboard score showing up (it didn't do anything)

0.7.27.7
====

Heads up when you install this update, the auto-restart won't work but it works again from now on

# New features
- Option to never show score screen when quitting out
- Option to never use rates when suggesting songs
- Added Letterboxing support as a new video mode under System settings
- You can choose a custom windowed resolution (for both letterboxed and windowed mode)
- Input meter has an option to flash indicating the judgements scored on each key
- You can change offset manually from practice mode (as well as the automatic tools)

# Improvements
- Offset can be changed from the context menu in level select, and remembers its suggestion between screens
- osu! Conversion now auto-corrects certain stacked notes instead of skipping the chart
- Keymode selectors are now dropdowns in all places
- 'Last played' indicator on level select updates immediately after playing
- Volume slider looks better
- Like/Unlike is now one hotkey instead of two

# Bug fixes
- Fixed PBs for the wrong ruleset showing if changing to a ruleset without PB data
- Fixed a crash when skipping a song
- Fixed a visual issue with explosions/column lighting in previews
- Fixed new users having an empty session on 1st January 1970

0.7.27.6
====

Road to 1.0

# New features
- Many more charts now have a leaderboard!
- Option for pacemaker to use the higher of your PB and your default target
- Your username is shown on the score screen
- Score's username is shown on the score screen when dropping an osu! replay
- Button to "cycle" texture frames when editing them in the Noteskin/HUD editor

# Improvements
- Improved some UI and menus around importing
- osu! skin conversion respects NoteWidthForHeight scale
- More accurate osu! scroll speed estimate
- Dropping an osu! replay will take you directly to the correct chart if you have it
- Right-clicking on a selected group opens bulk actions (before it opened the context menu as if unselected)

# Bug fixes
- Fixed a crash when using Windows emoji keyboard
- Fixed being unable to click on ruleset submenus due to hovering over local scores
- Fixed some parsing errors in timing points in very old .osu files
- Various fixes to mounted imports

0.7.27.5
====

A few bug fixes and features requested by you
Thanks again to the many new members of the Discord that have reported issues/suggested features :)

# New features
- Hold shift while hovering on the score graph to see stats for a "slice" of it
- BPM, NPS and CPS info are shown while hovering over timelines in previews and replays
- Rulesets emulating all Quaver judgements are now built-in defaults you can add via the ruleset menu

# Improvements
- Playing a playlist now respects the current search and filter entered in level select
- Updated the layout of the website a little (with plans to do more in the future)

# Bug fixes
- Fixed tiny pixel holes in the score line graph
- Fixed score graph window labels overlapping each other
- Fixed volume up/down keys not working on high framerates
- Fixed several visual bugs with the Judgement Counter
- Fixed editing timing windows on a cloned ruleset also updating the original

0.7.27.4
====

Yet another hotfix update

# Bug fixes
- Hotfix for a change to the osu!stable client's database format today, score importing should work again after this fix
- Reverted the anti-aliasing changes to framebuffers, it caused hard-to-investigate issues (in addition to what was fixed in 0.7.27.3) on other people's hardware

The reverted change for framebuffers only affects the score screen graph and gameplay previews, the MSAA setting and its performance gains remain the same

# Improvements
There is now only a "Log in with Discord" button instead of another one to register  
If you don't have an account it will prompt you to register automatically, otherwise it will log you in  
Multiple people (not naming names) were clicking "Log in" without an account and reporting it as a bug

0.7.27.3
====

Hotfix for a rendering issue on older hardware (thanks @Kori for reporting)

If the previous update caused your score screen graph to stop rendering, it should now work again

0.7.27.2
====

Had an influx of new users with the turn of the new year, thank you for playing :)

# New features and improvements
- HUD editor has been overhauled with an easier to use UI
- Many HUD elements have been renamed to something more fitting
- HUD elements in the editor have previews to help you find what you're looking for
- Many new hotkeys in HUD editor, including Ctrl-Z to undo
- Overhauled the appearance of the Judgement Counter HUD element
- Option to not save failed scores, can be found under Gameplay > Pacemaker
- Chart preview in level select now shows column lighting and explosions
- Pressing Ctrl-Z will reset sliders to how they were when you opened the menu

# Bug fixes
- Fixed XP bar on stats screen not also factoring in today's XP
- Fixed various issues in stats and score screen on 4:3 aspect ratio
- Fixed grades not auto-sorting in the ruleset editor
- Fixed a visual issue with combo line graph on the score screen
- Fixed an issue when scrolling backwards through a replay
- Fixed an issue when converting osu! skin fonts

# Engine improvements
- You can now adjust anti-aliasing under System > Performance and Framerate. Turning it off completely may give huge performance improvements on low end machines
- Font rendering has been improved to look much better even with anti-aliasing turned off completely
- Framebuffers now have built-in anti-aliasing (Improves look of score graph and gameplay previews in menus)

0.7.27.1
====

Happy new year

# Improvements
- When hovering over skill breakdown graph, an estimate of equivalent percentages for the current ruleset is also shown
- Sending a multiplayer invite has a success notification
- Importing osu! replays by dropping them onto the game lets you enter what rate the .osu file was on (still experimental)
- Minorly rearranged the import menu
- Column lighting now renders behind notes for parity with osu!mania

# Bug fixes
- Fixed some issues when importing osu! skins with bad paths in the skin.ini
- Improved osu! skin conversion for 'Percy LN' tails
- Optimisation to level select screen thanks to @9382
- Some changes to the audio engine that should reduce offset drift on rates (particularly downrates)
- Fixed an internal error when importing a .sm file with more than 10 keys

0.7.27
====

Stats overhaul update!  
(WHEN YOU SEE IT)

# Stats screen has been overhauled
- Browse through historical sessions (and see an activity graph of your playtime)
- See what scores were set in a session (and convert the session to a playlist)
- See overviews, timelines and breakdowns of your keymodes and skillsets
- New XP level system

Let me know your thoughts in the usual places, skillset info on LNs and more are planned

# Improvements
- Simplified the Noteskins/HUDs selection menu
- KPS meter updates less frequently which makes it easier to read

# Bug fixes
- 10k column filters no longer go off the screen on 4:3 screen sizes
- Fixed 'Open in browser' button in Etterna pack browser not being clickable
- Fixed ruleset ghost-tap judgements awarding combo
- Fixed keybinds sometimes triggering while typing in search boxes
- Fixed config files with missing options not being loadable
- Fixed several issues with .sm parsing
- Fixed a rare crash in input engine when restarting a song
- Fixed startup crash on macOS
- Fixed game taking a couple of seconds to close on CPU Saver mode
- Fixed game sometimes hanging on exit

0.7.26.11
====

Separating the engine update from the stats update was a good idea
Thanks to those in the Discord (especially @Kori) for reporting various visual issues which are now fixed

# Bug fixes
- Fixed background being transparent with 100% background dim
- Fixed background being transparent during the loading transition to another image
- Fixed a visual issue with LN bodies on most skins
- (Hopefully) fixed issue where the game sometimes hangs when exiting

# UI Improvements
- Added a hotkey for the stats screen (Ctrl+J default)
- You can scrub through the chart preview and replays with scroll wheel
- You can pause the chart preview with Space or Middle Click (both rebindable)
- Pressing the mod menu hotkey (S default) while mods menu is open closes it again

# Skinning
- Default text for Early/Late meter has been changed from "Fast"/"Slow" to "Early"/"Late"

0.7.26.10
====

Engine patch ahead of the when you see it update

# Engine changes
- Fixed Borderless Fullscreen not working correctly when the task bar is hidden
- Changed hotkeys to show FPS/performance overlay to F3 and F4
- The game no longer opens a console window on start, crashes open the log file directly
- Opening Interlude.exe while it's already running focuses the window
- Simplified lots of game engine internals

# Skinning
- Timing display now supports being rotated sideways
- Fixed bug where black background was not removed from osu!-converted explosions

# Other
- Stat tracking has been overhauled internally for a new feature coming out in the next few days..
- Press tab while editing ruleset timing windows to auto-select next input box
- Various localisation fixes
- Improved look of local scores

0.7.26.9
====

Bug fix and quality of life update. Enjoy :)

# New features
- You can now edit (most) hold mechanics in the ingame ruleset editor
- Noteskins support keymode-specific column widths
- With pacemaker on, you can now fail a song if you miss your target
- Failed/quit scores can be saved and viewed like any other score

# Improvements
- Some UI improvements for: changing gameplay binds, local scores list, online player list
- You can choose what OD/HP to use when exporting charts as .osz
- Grouping by date played/installed works on actual "day boundaries" rather than rounding how many minutes ago to an integer
- Pattern calculation has changed slightly again

# Bug fixes
- Fixed alignment of background image during the song start overlay
- Fixed pacemaker not working if set to use your lamp PB, and your PB is NONE
- Rulesets with the same name no longer overwrite each other
- Fixed several bugs where switching rulesets in certain scenarios doesn't work

0.7.26.8
====

Major rulesets update!

Rulesets have been rewritten and **your old rulesets will no longer load**.  
In exchange, the ruleset editor is much more powerful so you'll be able to recreate your ruleset in about 10 minutes

# New ruleset features
- Built in osu!mania rulesets are now extremely accurate to the stable client, rice notes on nomod in particular are 100% exact to what osu! would give you
- New option for rulesets to display accuracy to 3 or 4 decimal places instead of 2
- New option for rulesets to award a penalty judgement when you ghost tap
- Ruleset editor now allows you to add/remove judgements, customise timing windows and more without editing the .ruleset directly
- Ability to clone a ruleset

The rewrite has had some other knock-on improvements to other connected features

# Other new features
- Score graph data is now more granular and hover info is more accurate to your mouse position
- Songs you click through on level select are added to history, shift + R takes you back
- Improvements to pattern analysis and chart categorisation
- New 'annotations' overlay when viewing a replay, showing exact ms deviations of hits, ghost taps and more
- Replay mode has an option to keep the same perceived scroll speed when changing playback speed

# Bug fixes
- Fixed dozens of small issues where times would not scale on rates across various HUD elements and animations
- Fixed a bug causing performance rating to scale incorrectly on rates
- Fixed exporting charts to osu! not normalising BPMs correctly
- Playlists in level select now show PB info for the rate set by the playlist

0.7.26.7
====

Fixes a crash when attempting to add grades/lamps to a ruleset  
That is all, more stuff coming soon :)

0.7.26.6
====

Bug fix update + Some minor feature requests

# New features
- Ruleset editor allows you to set 4 decimal places for grade accuracy requirements
- Button to open rulesets folder when managing rulesets
- Timing display can now show timing windows as an underlay
- Input meter has more customisation around key transparency and padding
- Clicking the forward button on the jukebox restores songs you went "back" from instead of always picking a new random song
- Packs with numbers in will now sort correctly by those numbers e.g. Pack 1 < Pack 9 < Pack 10

# Bug fixes
- Fixed an issue with parsing osu! skin.inis that affected skins targeting only one keymode
- Fixed sliders sometimes displaying '-0%'
- Prevented deleting the SC (J4) ruleset which could cause a crash
- Fix BPM indicator on level select not updating when you change rate
- Fixed text clipping on some buttons

0.7.26.5
====

# Multi-select and bulk actions

In level select, you can now select multiple charts/groups by holding shift + clicking  

After doing so, there is a bulk actions button that lets you do stuff like:  

- Deleting multiple charts at once
- Exporting multiple charts to .osz (use with caution when exporting a lot)
- Adding a batch of charts to a collection/likes

# Other new stuff
- Skip button has been improved to take you closer to the first note
- Transition into gameplay now fades the audio
- Info card at the start of gameplay can be dismissed by pressing the skip key
- You can export as .osz from the score screen chart actions

# Bug fixes
- Fixed a chart database issue that broke @Lifly's game. **If you also had this issue, you need to download this update manually and swap your Interlude.exe for the new one**.
- Fixed a pixel seam in screen transitions

0.7.26.4
====

It's like a whole new game (again)

# Skinning
- Added a new 'Judgement line' element, it's a skinnable texture that stretches over your whole hitposition area like osu's `mania-stage-hint`
- Receptors can be turned off, if you only want to use the judgement line, or having nothing there at all
- Improved osu! Skin conversion including auto-converting judgement lines
- Column light colors can now be customised per keymode + vertically offset
- Receptor colors can now be customised per keymode
- Option for notes to always go under receptors

# Chart database

Your charts are now stored in an SQLite database file instead of as folders of .yav files

This means:

- Faster disk operations
- I got to clean up and improve a lot of code
- It should be impossible for the cache to break and need a recache any more
- It paved the way for some new level select features (see below)

# New level select features
- Ability to 'like' charts - They appear when viewing collections and act like a folder
- Toggle on level select to always show collections at the top alongside other groups/search results
- Toggle on level select to show native (normally Japanese) artists and titles where available

0.7.26.3
====

Hotfix for level select being broken and not highlighting what you have selected correctly

Also fixes an issue with .osu files with decimal timestamps not converting correctly

0.7.26.2
====

Bug fix update :)

# Bug fixes
- Fixed game's viewport being cut off at the top when launching in windowed mode
- Fixed keybind conflict between reset offset and deleting characters when typing an offset

# New features
- Option to export charts to .osz with current mods applied
- KPS meter is now a separate HUD element
- New audio config options to correct crackling audio on some Linux systems
- You can use R and SHIFT+R while the preview is open to cycle through random charts (before, the preview had to be closed)
- some other stuff. it's a secret

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

0.7.26
====

It's like a whole new game

# UI improvements
- Tooltips now show on mouse hover, rather than needing you to press /
- / shows tooltips for the keyboard-selected element, allowing you to use tooltips without a mouse
- Redesigned replay screen
- Score screen has been adjusted to look better
- Start of gameplay has a transition which shows song info + pacemaker info
- You can now type numbers into options menu sliders
- Menus apply a muffling effect to the song which I think is really cool

# Other features/improvements
- You can now edit rulesets ingame, it doesn't cover everything but it does cover the main things like colors and names. More coming soon
- Revamped skin textures so textures don't need accompanying .json files. Eliminates a bunch of common user mistakes :)
- Autoplay now shows input meter/some other HUD elements it didn't before
- CTRL+SHIFT+R skips the current song and immediately goes to the next suggestion

# Bug fixes
- Fixed more bugs with rulesets (osu! OD0 + EZ, Wife lamp colors)
- Fixed backtick character having no glyph
- Fixed some issues with LN tail rendering + scroll directions
- Fixed a parsing issue with osu! skin conversions
- Lots of other small things

0.7.25.1
====

Bug fix update

# Bugs fixed
- Fixed dots on score graph being slightly vertically off
- 'Continue' button shows notification when it failed to find anything, instead of doing nothing
- Fix wife lamp colors and J9 scoring curve (again)
- Fix KPS meter showing 0.5x the right number

# Changes
- Endless mode now suggests things on rates (so that on that rate, it's similar to what you just played)
- Endless mode should now suggest better stuff
- Moved preview/mods/rulesets buttons on level select back where they were

# New stuff
- `pattern=...` in level select can now pick out particular patterns e.g `pattern=quadstream`

This update will need to recalculate patterns for all installed charts. This is automatically done when you update

0.7.25
====

# Endless mode
Endless mode has been overhauled: Now just press 'Continue' on the score screen to play another song

# Other new things
- Retry button also added to score screen
- Some changes to level select UI, let me know what you think
- 'Custom image' HUD element that can also be animated
- Sort by grade and lamp on level select
- Scroll wheel on score graph to zoom in, graph is also bigger now
- Option to draw score graph line over hits instead of under
- Option to draw timing window overlay on score graph
- Changing local offset is a menu again

# Bug fixes
- Fixed KPS meter not scaling to rates
- Fixed J9 windows not being the right scale (requires regenerating the ruleset)
- Fixed Wife AAAA and AAAAA lamps not being the right color (requires regenerating the ruleset)
- Fixed score graph disappearing when the window is tabbed out or resized
- Fixed not being able to pause on replay screen

0.7.24.8
====

Hotfix update

# Bugs fixed
- Fixed ruleset selection appearing behind score screen graph
- Fixed import indicator being too high up

# Other stuff
- You can now search "orbs", "arrows", "bars", etc in the skins browser
- Changed appearance of the ingame wiki and tooltips slightly
- Added MA/PA line graph modes
- Some other subtle UI improvements

0.7.24.7
====

I have honored several feature requests

# New stuff
- 'Receptor offset' setting in noteskins
- Added a 'KPS meter' option to the Input meter HUD element
- Rates up to 3.00x are now supported (previously went up to 2.00x)
- Sort by 'Date played'
- Click on the score screen graph to expand it
- Message shown when trying to reload skins in a menu, saying to close the menu first

# Improvements
- osu! Beatmap search is now powered by Mino which should work significantly better
- Fixed a bug when exporting certain charts to .osz
- Noteskin editing menu has been rearranged to make things easier to find
- Pacemaker HUD element updates its position more often

Those of you in the Discord that know about the skillset breakdown system I've been working on:  
You can now go to 'Skillsets' on the Stats page to snoop at your graphs and numbers, but you otherwise can't interact with it yet

0.7.24.6
====

Some cool technical changes in this one, let me know in Discord if you experience any issues

# New stuff
- If you are on Windows, the Windows key is disabled during gameplay (so pressing it by mistake won't ruin your score)
- Console window is now hidden except when the game crashes
- High accuracies are formatted to more than 2 decimal places (i.e. accuracies between 99.98% and 100.00%)
- Score screen column filters now also affect the left-hand-side stats
- Added text to the Quick menu indicating the hotkey
- Standard Deviation line mode on score graph now scales to highest SD during the score

# Bugs fixed
- Etterna pack downloader updated to point to the new site (no longer beta.etternaonline)
- Timestamps on timelines have been adjusted to make more sense
- UI crashes no longer instant-quit the entire program, you can see the error message like you could a long time ago
- Fixed an issue with transparency + fading on the input meter

More cool features in the works, those of you snooping at code commits will know more than I let on in these changelogs for now

0.7.24.5
====

I'm away most of this weekend so have this small update

# New stuff
- New VERY EXPERIMENTAL secret tool to import Etterna noteskins, drag and drop a noteskin folder onto the game to try it
- Mode on score graph to show accuracy as a line graph
- Option to change line graph color to match grade instead of lamp
- New 'Quick menu' where common actions will be placed, this replaces the home tab in Options
- Hold CTRL + SHIFT + (plus or minus) to change rates by +-25%

# Bug fixes
- Fixed lots of rate-modded beatmaps where the offset has also been edited getting past the import filter

0.7.24.4
====

Bug fix update :) Enjoy

# Bug fixes
- Fixed visual bugs when song background image has transparency in it
- Rate-modded beatmaps are now discarded if the import also contains the 1.00x version, but kept otherwise

0.7.24.3
====

Various small features requested by you

PLEASE NOTE I have made a technical change to how personal bests are stored (will allow more cool features)  
To recalculate your PBs on level select, go to Options > Library > Recalculate scores and let it run in the background for a few minutes

# New stuff
- More information is shown when you hover on the score screen graph
- Options to draw standard deviation/mean as the score graph line instead of combo
- Options to filter the score graph dots by columns
- Improved UI for selecting fullscreen resolutions and refresh rates
- Mod select now shows what effect your selection has on score ranking/saving
- Right-click drag scrolling on level select has been changed to be more like it used to
- New level select sorts: By length and by date added
- Scores using the Shuffle and Random mods are now saved
- The pacemaker will compare you to your live replay from your personal best rather than just using the final accuracy as a target

# Bugs fixed
- Fixed preset buttons having too small of a clickable area
- Fixed note colors sometimes not updating correctly when changing presets
- Fixed error in standard deviation and mean calculations on score screen

0.7.24.2
====

Bug fix update for audio issues  
Also gives me practice with my new development strategy that lets me put out bug fixes independent of longer running feature work

# Bug fixes
- Fixed changing songs not respecting the selected audio device
- Fixed sound effects muting after changing audio device

# Improvements
- Ctrl-Alt-Shift-S now reloads the theme, *all skins*, and all rulesets
- Importing a chart by any means (dropping a file, ingame imports, etc) will now correctly select it in a multiplayer lobby if it was missing
- Some other tiny changes not worth listing out

0.7.24.1
====

Bit of a weird follow up update but here you go

# New feature
You can now turn off pitch rates via Options > System > Audio! When turned off, rates no longer also pitch the song up/down

Let me know if you experience any bugs or offset issues while using this

# Bug fixes
- Fixed a bunch of visual problems with hud elements in previews
- Fixed some bugs with scrobbling on timelines
- Fixed a bug when importing a .isk by dropping it on the window
- Fixed problems clicking dropdowns in the HUD editor

0.7.24
====

*Skinning update*

Noteskins and HUD layouts have become separate things, and a Skin is a new container folder that can hold one or both of these things
An .isk file now represets a Skin (so it can contain a noteskin, hud layout or both)

You don't need to do anything, your existing noteskins should be turned automatically into new Skin folders

# New features
- You can now select which HUD and which Noteskin to use independently of each other
- Timelines and score graphs now display timestamps
- Options menu previews now display HUD elements, explosions and column lighting
- You can set negative column spacings and hold trim values ingame instead of manually editing noteskin files

# Many UI improvements
- Hold backspace to delete text
- You can re-download Etterna packs that are already installed
- The 'Import' button now opens a menu instead of a dropdown
- Presets UI has been updated and improved
- A bunch of other really tiny but nice changes

0.7.23.1
====

Several bug fixes thanks to your quick feedback :)

# Bug fixes by me
- Fixed some visual issues in the Input Meter fade effect thanks to Lylcaruis
- Fixed bug converting some older Quaver files thanks to 9382
- Fixed behaviour of mouse dragging on color pickers thanks to Lylcaruis
- Fixed Input Meter speeding up/slowing down when on rates thanks to Lylcaruis

# Bug fixes by @9382
- Fixed audio pausing when changing songs too quickly
- Fixed UI overlapping on prompt about importing an unlinked songs folder

0.7.23
====

Major UI overhaul!  
I have done about 1/3 of the changes I want to do, but releasing at this stage so you can tell me what you think so far

Please report any bugs or suggestions in the Discord, and enjoy :)

# UI overhauling done
- You can search for hotkeys
- New redesigned menus for importing osu! beatmaps, Etterna packs, rulesets, tables and noteskins
- Import screen is gone and all these features are part of the options menu instead
- Some other general changes to how menu pages animate and look
- Ingame wiki and changelog are now menu "pages" and can be opened while already inside options menus

# New features
- Scroll speed now has an indicator of ms reaction time alongside game conversions
- Score screen shows MA and PA ratios
- Many more settings for the Input Meter HUD element
- New 'Guide thickness' setting for the Timing Display HUD element

# Bug fixes and changes
- Fixed localisation not working on hotkeys menu
- Miss penalty for SC on judges other than 4 has been adjusted to scale with their difficulty

0.7.22
====

This update is pretty fire imo

# New features
- Middle click to pause music on menus
- Right click on charts to export as .osz
- New 'Input meter' HUD element
- SV indicator on pattern summaries
- You can now batch-import a folder of .osz files (like coro's 7k megapacks once the .rar has been extracted)

# Changes and improvements
- UI for the ingame wiki and changelog has been improved
- Imports log which files were skipped and why
- Cursor is hidden during practice mode and replay mode
- Table suggestions UI has been improved
- osu! rulesets have been changed to support decimal OD values, Hard Rock and Easy mods
- Improved screen transition animations when entering/leaving gameplay

# Bug fixes
- Ingame Etterna downloads now point to the new beta site since the old one is now gone
- Fixed several parse errors and bugs in imports, osu! and Etterna alike
- Fixed mismatched note sizes when importing osu! skins
- Fixed UI for selecting keymodes on presets being invisible
- Fixed downloading an update to a table requiring game restart
- Fixed endless mode dropdown not being usable with a mouse

0.7.21.3
====

More important bug fixes, thanks again to playtesters of the new features

# Bugs fixed
- Fixed a crash when giving up halfway through a song
- Fixed some Quaver SV charts not rendering the same as Quaver would (but there are still a couple of unusual cases to look into)

# New features
- New moving average modes to the timing display/error bars HUD element
- When on, arrow moving average is displayed above the error bars
- When on, bar moving average replaces the error bars with a moving bar

0.7.21.2
====

Another bug fix update to solve some fairly big bugs  
Big enough that I want to release it sooner, with another feature update probably on Tuesday

# Bugs fixed
- Improved security of account login tokens
- Drastically reduced CPU usage of the game engine in general (related to inputs)
- Fixed an issue in multiplayer that would mess up replay data + updated the multiplayer protocol with a better solution
- Fixed being able to use the jukebox to change songs from the multiplayer lobby screen
- Fixed being able to use the jukebox to change songs while playing (crashing the game)
- Fixed one last visual bug with local/online scores being blank depending on how you switch around the tabs

# Improvements
- You can now use `#sv` and `#nsv` in the search bar to respectively find SV and non-SV charts
- Gameplay binds and hotkeys now show up in search if you type 'keybinds' along with a few other related keywords

0.7.21.1
====

Patch update to resolve some reported bugs with chart conversions
Also this is me double checking that my auto-update rollout system is fixed after what happened earlier today

# Bug fixes
- Fixed certain badly formatted .sm files from converting as corrupt .yav files (and for Newson, a recache will clean out any of your existing broken files)
- Fixed flickering visual issues when rendering Quaver files with negative SV
- Fixed InitialScrollVelocity being ignored when converting Quaver files

Thanks again to people quick to the draw on playtesting the Quaver conversion, let me know if there are other issues

0.7.21
====

Big feature update  
Let me know which new feature is your favourite

# Quaver support
- You can now import all your favourite Quaver charts into Interlude
- This includes mounting your library like for osu! and Etterna
- Negative SVs are supported!

If you find a bug with any Quaver conversions let me know in the Discord.

# Other features
- Added a jukebox in the bottom left of the toolbar that lets you pause and move forward/back through songs
- When on the main menu, the jukebox will cycle through songs when the previous one finishes
- Hotkey to go forward is R, Hotkey to go back is Shift+R and the jukebox is connected directly to the random suggestions system
- Shift+R takes you to the previous chart from before you pressed R, or previous charts from endless mode
- Added an option for the lane cover to be underneath your receptors, explosions and column lighting
- Exiting a song after playing more than 15 seconds is treated as "giving up" and shows you a score screen for your partial play
- Options menu now has a search box that lets you quickly search for settings

Whenever you expected a search to find something let me know, I will expand what keywords match to what settings over time.  

# Improvements
- Pattern breakdown is now shown under local scores for improved visibility
- If you only have 1 ruleset installed, the quick-switcher has a button to 'Get more rulesets' taking you to the import screen

# Bug fixes
- Fixed an error when recaching charts reported by Newson
- Fixed table suggestions UI being unviewable
- Fixed osu! charts with stacked notes converting to corrupt files (now they don't convert at all)

Enjoy!  
There may be a part 2 to this update later this bank holiday weekend, adding more features I've discussed in Discord

0.7.20.4
====

Hotfix for names not displaying correctly on mod menu :)

0.7.20.3
====

Bug fix update, enjoy
I did a load of refactoring too

# Bug fixes
- Fixed a crash when a chart fails to load while no chart was currently loaded
- Fixed not being able to retry in practice mode while playing
- Fixed not being able to unpause replays when at the start of the song
- Fixed visual issue with hit overlay on replays
- Fixed being able to interact with/click on UI elements that are visually under notifications
- Fixed some issues with dropdowns closing when hovering over another UI element behind it
- Fixed accuracy 'glint' effect on score screen only playing if you went up a grade
- Fixed game constantly reconnecting to server if out of date

# Improvements
- UI for setting up a mount of your osu!/Etterna/StepMania library is much better
- Made local scores a bit more compact to make room for the future
- Inverse mod has modes with different gap sizes
- Inverse mod can be used with NoLN to produce FLN charts
- Logo waveform looks better
- Input overlay on replays now has visual indicator of what a perfect hit would be

0.7.20.2
====

Various tiny quality of life improvements  
The ingame wiki has also been updated to reflect newer information

# New features
- No holds mod now has alternate modes that remove LNs shorter than a certain length
- In multiplayer, charts you don't have will automatically download if available server-side
- You can filter by LN% in level select by typing `ln>0.5`, `ln<0.8`, etc

# Improvements
- Ruleset selector now groups rulesets of the same type as a submenu to reduce scrolling
- Progress meter now fills in smoothly instead of in segments
- Added a glint effect on the score screen when you set a personal best

# Bug fixes
- Fixed a visual bug with the label of the progress meter
- Fixed a visual bug with the judgement counter
- Fixed a rare crash to do with explosion animations

0.7.20.1
====

Patch update as an excuse for me to test out my new CI tools and make sure I didn't break anything

# Bug fixes
- Swapped to Mino for osu! beatmap downloads as chimu.moe is down
- Fixed osu! skin import list not scrolling when you have many

# Improvements
In the 'Textures' tab of the noteskin editor, you can now toggle between textures stored as a grid or as loose files  
This is to suit your preference depending on how you like to edit/distribute images  

In future I plan on enforcing grid textures for zipped .isks as they load faster and it's easier to copy paste a texture as a single grid from them

0.7.20
====

Major noteskin update
Documentation and more sample skins coming this weekend

# New features
- Custom font support for accuracy meter, progress meter, judgement counter
- Custom background image support for skip button
- osu! Skin converter can be accessed from the imports menu rather than just when dropping a skin onto the window
- Option to delete/replace imported skin with a new version when importing
- You can open the options menu from the score screen (for endless mode)
- You can delete charts from the score screen (for endless mode)

# Improvements
- osu! Skin converter converts explosions + fonts from osu skins
- Ability to open skin folder/delete skin when editing it

# Bug fixes
- osu! song imports ingame are fixed
- Added protection from users running Interlude from a zipped archive
- osu! Skin converter converts receptors better and not vertically offset
- osu! Skin converter converts 'percy' long notes better

0.7.19.8
====

I'm running out of things to say in these headlines  
Soon these updates could do with screenshots/some prettier blog-style post showcasing what's new

Due to several bug reports coming in from people breaking multiplayer - you know who you are :) - I've refactored some of the code to improve its stability  
Multiplayer should be more stable rather than less, but please report any more bugs you find

# Multiplayer
- Lobbies start with auto-countdown turned on by default
- You can no longer rename your lobby to invalid names like the empty string
- Fixed a bug where you can join a lobby twice, breaking it for everyone
- In general I have removed exceptions from many execution paths and replaced them with graceful handling of unexpected cases

# New features & Improvements
- 'Smart' frame cap has been overhauled again and should work better for everyone, try it again if you're using 'Unlimited'
- Noteskins now support custom fonts for the combo indicator, like osu! does
- osu! skin converter now attempts to port 'percied' LN bodies correctly
- osu! skin converter now ports combo fonts
- Pacemaker has an option to automatically use your PB if available, instead of the fixed value
- Keybind to change local offset is now distinct from the options menu key

# Bug fixes
- osu! skin converter ports column lighting better
- Fixed some crashes and issues in noteskin loading/reloading
- Fixed some issues with tabbing out of fullscreen
- Fixed a long-standing graphical issue with LN tails

0.7.19.7
====

This update should fix the antivirus issues some people were experiencing this week

# New features
- Noteskins now (optionally) support a different receptor texture per column
- osu! Skin converter now converts key textures + column lighting correctly
- New level select buttons for entering endless mode + other actions
- 'Problems' tab on noteskin editor that lists potential errors with your noteskin (and can help you fix them)
- Option to practice a chart appears on the context menus of charts

# Improvements
- Game's icon has been changed to match newer branding of the site and discord
- Endless mode now tries to closely match the BPM/patterns when giving suggestions
- Options menu has been slightly reorganised
- Interlude.exe size has been reduced significantly

# Bug fixes
- Fixed two bugs in converting judgement textures from osu! skins
- Removed parts of Interlude.exe that were causing false positives in some antivirus software

0.7.19.6
====

Yet Another Minor Feature Update

# New features
- You can now play a playlist onwards from any point
- New 'Noodles' ruleset available on the imports screen, for practising LN releases
- New optional noteskin feature to have a custom background texture behind the judgement counter

# Improvements
- Various improvements to the HUD editor, most notable: you can right click on elements to open their settings
- More various improvements to the noteskin texture editor
- I tidied up some code in many places

# Bug fixes
- Fixed a minor issue when osu! skin converter converts tall LN textures
- Fixed explosion animations glitching out when a song loops during practice mode/editing HUD

0.7.19.5
====

I have not heard complaints about the new smart frame cap so that is good  
Here are some more features :)

# New gameplay features
- 'Shuffle' and 'Randomise' modifiers, check them out in the mods menu for more info
- Option under pacemaker settings to only save scores that are a new record of some form

# Bug fixes
- Fixed pressing enter sometimes not playing the chart for some reason (I know the exact reason)
- Fixed a bug causing osu! skin converter to use different textures to what osu! stable does
- Fixed mod meter showing the wrong mods when watching replays

# Other improvements
- Text appears in top right reminding users of Ctrl-O to change local offset
- Slight improvements to ingame noteskin texture editor

0.7.19.4
====

Slightly delayed feature update  
I also redesigned the site a little, go check it out :)

# Smart frame cap overhaul
It should now run a lot smoother on more hardware, but needs some field testing  

If the game now runs like absolute butter for you please let me know and applaud my genius in the Discord  
If the game now runs worse for you please immediately complain in the Discord and I can take a look  

# Noteskin and HUD features
- Early/late meter now supports noteskin textures the same way the judgement meter does

# Other features
- More informational toasts/notifications for various things such as when an osu! skin.ini fails to parse
- osu! skin converter now converts judgement textures over to noteskins
- New gameplay modifier: More notes -- This is experimental and subject to change, therefore you cannot save scores with it on
- Discord rich presence activity now has an icon

# Bug fixes
- Fixed background images being smeared horizontally if the game initialised while minimised in fullscreen
- Fixed LN% and note counts not updating when changing selected mods
- Fixed a crash in the HUD editor nobody knew about

0.7.19.3
====

As usual thank you players new and old for trying the game and giving feedback :)

# Noteskin and HUD features
- Noteskins now support custom textures for judgement indicators! Skinners rejoice - Template skins, documentation, etc coming soon
- Judgement meter has an improved animation, which can be turned off if you want to use a texture-only animation
- HUD editor is part of the noteskin editor + improved navigation to it
- Hold shift + use arrow keys to make 1-pixel adjustments in the HUD editor

# UI improvements
- Audio rates can be changed with a slider from the mods menu, to help more people find it
- Ruleset switcher now correctly scrolls instead of clipping offscreen when you have lots installed
- Score screen now shows who made the chart
- Score screen slightly adjusted to look better
- All menus have been overall re-scaled and adjusted to look more consistent

# Bug fixes
- Fixed a crash when viewing the tooltip on the ruleset picker
- Fixed several issues with Smart frame cap on multi-monitor setups
- Fixed parts of main menu disappearing if you tab out in fullscreen while it loads
- Fixed hotkey for reloading themes/noteskins not reloading certain config about textures

Lots of UI code was changed for this update, I might have missed something  
If you spot anything that looks wrong please report it

0.7.19.2
====

# Noteskinning update

A large portion of HUD configuration has been **moved to noteskins**
This means that noteskins are now in control of things like:

- The position of HUD elements
- The appearance of HUD elements

Some settings that I think make more sense as user preference are **still part of the normal settings** and are **saved independently from noteskins** such as:

- Which bits of the HUD are turned on
- Special HUD behaviours like the label on the progress meter
- Preferences like lane covers

Please note that your existing HUD settings will be reset by this update - The original config files will not be deleted in case you need to refer to them

# What's new
- New ingame editor for positioning your HUD elements! You can now click and drag OR use arrow keys
  Please break it and report bugs, I will do a follow up patch based on your feedback
- Menuing around customising HUD elements ha been redesigned a bit to explain these changes around noteskins

# Bug fixes
- Fixed "Delete preset" button not having a position set
- Improved loading speed of local scores scoreboard
- Finishing importing a pack/song should no longer lag spike gameplay screens as you play
- Localisation fixes and improvements including some from @ZeNyfh :)
- Fixed crash when a noteskin has no textures

# Misc
- Spamming the screenshot key on the score screen now throws some confetti after taking 1 screenshot

0.7.19.1
====

Fixes some issues and cut corners in pattern analysis based on this morning's feedback

# Pattern analysis changes
- Fixed a bug causing the game to load pattern data slower than it should
- More charts categorised as "Jack" now correct get categorised as "Chordjack"
- Fixed a bug where the score screen just shows patterns for your current chart + rate, not for the score
- Added a tip about searching for patterns on the search box tooltip

0.7.19
====

Lots of new technical stuff and a big leap forward in pattern analysis!

# New score database
Scores are now saved in a real database file using SQLite instead of being a flat file

This has a few benefits

- WAY faster loading and saving
- Significantly more reliable against abnormal crashes like blue screening or power outage
- If you have SQLite browsing software you can now look at the data and make queries :)

The upgrade will be done automatically, afterwards you can delete your old score.json files

# Pattern analysis
Added a new "summary"/categorisation to all charts giving a rough idea of what's in it  
This category also comes with a breakdown of what the major components of the chart are and any relevant minor ones

The idea of this is to be less fine-grained than the details tab currently on level select but enough to be really game-changingly useful for finding what to play

With this update

- Pattern analysis is now turned on for everyone, not just a hidden feature in Advanced settings
- Group by Pattern on level select is now very good, please try it and give me feedback
- Pattern summary is displayed on that blank spot on the score screen

# Other changes
- Mass improvements to the layout of the codebase (particularly Prelude)

0.7.18.7
====

Small noteskin update, needed to release changes I've done so far so that the ingame noteskin repo works again

# New features
- Noteskin list on import screen now groups skins by versions and lets you browse which version you want
- Noteskin selection on options menu now indicates noteskin versions
- Some stuff to indicate original creator of a noteskin vs someone who made an edit to it

# Bug fixes
- `holdtail` texture is no longer required in a noteskin if `UseHoldTailTexture` is false
- Fixed a crash when installing a skin you already have installed
- Some improvements to keyboard navigating a container after hovering over something in it

# Technical stuff
- Some core libraries are now built against .NET 8 instead of .NET standard 2.1
- Improved how files download after getting rid of some outdated code for .NET standard

0.7.18.6
====

This update has nothing new, it's just me testing that the release pipeline still works after the ImageSharp CVE caused build warnings to appear

I will have to upgrade ImageSharp to a patched version at some point but for now feel free to execute as much arbitrary code on your own machine as you like

This update also includes updating GLFW 3.3.7 -> 3.3.8 which looks like it contains fixes for the segfaults on M1/M2
Perhaps this will mean I can start doing official releases for macOS?

0.7.18.5
====

I've mostly done internal code changes this week
To stick to the frequent release schedule I've been doing, here are some small things before those code changes pay off

# Bug fixes
- Watching a chart in auto-play mode no longer counts as "playing" it for the database/sort by recently played view
- Improved how LN bodies convert from osu! -> Interlude in many cases

# Skinning
- Improved automatic validation of noteskins under the hood
- Improved the osu! -> Interlude noteskin converter:
	- Option to convert arrow skins correctly
	- Converter attempts to convert stage-left and stage-right
	- Converter attempts to convert column lighting
- Improved guidance on the import screen towards importing your own osu! skin
- Can enable/disable stage textures in-game

# Endless mode
- Many changes under the hood that will pay off soon
- Playing a playlist has changed
	- Right click on the playlist or any song in it to play (instead of turning on endless mode and then playing)
	- New option to play it in a shuffled order
	
As usual stay tuned in the Discord for discussions and updates on what I'm working on next

0.7.18.4
====

Dropping an update before I move house, containing even more stuff directly from user feedback this week

# Bug fixes
- Fixed hold explosions continuing forever if you start holding an LN and never release the key

# Improvements
- Explosion scale soft cap lifted from 200% to 500%
- Huge stride/improvement in pattern analysis and recommendation algorithm again
  You will have to recache patterns once again to use endless mode
- (Advanced) setting to disable the confirmation to exit the game

# New features
- Vertical offset setting for explosions, allowing for a little more flexibility in skinning
- (Advanced) setting to hold esc/ctrl R to exit/retry (for players with switches that rattle or keep accidentally bumping these keys)
- Column lighting technically now supports different images per column, customisation in-UI coming later
- Scroll speed setting shows approximate scroll speeds as they would be in other VSRGs

0.7.18.3
====

Thank you to all the new players who have given me plenty of feedback, bug reports and ideas

# Bug fixes
- Fixed several bugs in mouse interactions with some options menus
- Fixed color pickers overlapping with other components in some pages

# Improvements
- Improved appearance of Noteskins selection menu
- Improved appearance of previews in all options menus
- Prompt before exiting the game
- Some invisible UX fixes you won't even notice.. :)

# New features
- Explosion animations have received a rework! You can now do many more powerful and nicer looking things with them

Due to the explosion rework, column lighting and explosion animation speeds have been reset for all skins, you may need to set them again to your preference.

0.7.18.2
====

I've rewritten some parts of how the mouse interacts with UI elements based on recent feedback
If you notice any unusual behaviour or bugs as a result please let me know

# Improvements
- All buttons/hoverables should no longer feel "sticky" when hovered with the mouse - moving the mouse away will "unhover" them
- All buttons/hoverables will still be sticky-highlighted when hovered using keyboard controls, working as before
- Went and tweaked/improved several tiny details of UI to work better
- Multiplayer chat should now correctly scroll to show the latest message
- You can now hold escape to exit out of nested menu pages so you don't have to spam the button
- Hold note combo bug in osu!mania ruleset reported by CommandoBlack should now be fixed
- Background images signficantly bigger than the game window are downscaled for performance

Some improvements to noteskin explosion animations are planned for next release

0.7.18.1
====

Hotfix for users outside of the UK (I love .NET standard library)

0.7.18
====

Hey I have some new stuff for You

# New features and UI improvements
- Playlists now display how long they last if played back to back
- Improvements to practice mode so it feels less cluttered
- You can now take screenshots even in menus and on the chart preview
- Some upgrades to pattern analysis in progress, this update gets you a sneak peek at the halfway point
- Discord Rich Prescence
- Clicking on a text entry will keep it focused until you click something else

# Bug fixes
- Fixed bug in how timestamps are saved/converted from UTC
- Fixed ingame progress timer showing -1:-1 as a song ends
- Fixed the LN rendering glitch when seeking to an earlier point in a song in previews and practice mode
- Possibly fixed being "Not logged in" even though the logs say you logged in fine

More stuff coming soon but I am busy this weekend and moving house the next so we'll see

0.7.17
====

Still on the UI improvement grindset, but also some new features :o

# UI improvements
- Improved the appearance of dropdowns, and dropdowns now correctly open with the right thing selected
- Game automatically retries connecting to the server (once) if disconnected
- Search bar in level select has been made slightly longer to look better
- Folders in level select are slimmer, have icons and show how many charts are in them

# New features
- You can change rates while paused in practice mode
- You can add charts to collections from the score screen (for endless mode users)
- You can play your playlists back-to-back by enabling endless mode and then playing a chart from your playlist in Collections view

0.7.16.3
====

This update patches a UI bug when selecting fullscreen resolution/mode

It also includes some performance improvements (your mileage my vary) when loading and saving scores to disk

0.7.16.2
====

I mostly did some more UI improvements :)

# Improvements
- New skinning feature! You can now add `stageleft` and `stageright` textures to your playfield via a noteskin  
  Currently this can't be done via the ingame UI, you will need to manually enable it in noteskin.json + add the stageleft.png and stageright.png manually  
  Contact me in Discord for a detailed tutorial
- Improved the UI for import screen even more (including a pretty slick indicator when an import is in progress)
- Rearranged the UI for noteskin selection
- Fixed a crash when running a freshly installed Interlude on your computer
- Fixed some offset issues when you have two local versions of the same chart, with different cropped length of audio files

0.7.16.1
====

UI improvements patch

# New stuff
- Some UI improvements to the ingame wiki and import screen
- The UI to suggest charts for tables is back (but still under construction)

0.7.16
====

Some pretty major work this update, but more coming on the way to finally Finish Tables

# Codebase changes
- Reorganised the logic of modules initialising so it is followable (resulting in less future bugs)
- Refactored lots of things to do with chart loading to be less complex (resulting in less future bugs)

These changes may have caused a new bug or two short-term but it's better to just release and then make easy patches as I go

# Bug fixes
- Due to the above some super obscure bugs around watching a replay with different mods to the ones you have selected are fixed
- Due to the above there are fewer obscure circumstances that crash the game
- Fixed a crash when opening the editor for a noteskin texture that doesn't actually exist as a file (still crashes if you try to edit it, for now)

Table ratings server-side are also fixed, you will have to set a new personal best to get an updated rating

# Feature changes
- I've rewritten tables basically from the ground up
- New UI for managing table downloads, so you can download just the sections or levels relevant to your skill level and not everything

You will have to reinstall Crescent via the Imports tab

If you have the charts downloaded you don't need to download them again, just the data file

0.7.15.5
====

Minor patch

# Improvements in this update
- Fixed a crash in multiplayer lobbies
- Fixed a crash related to previewing your noteskin right after making changes
- Fixed a crash on debug mode/linux in the user registration UI
- Added hotkeys to adjust audio volume (hold Alt + Up, Alt + Down), improving the keyboard-only experience :)

0.7.15.4
====

Another minor patch amidst more infrastructure changes

Interlude now runs on .NET 8 :)

# Improvements in this update
- Reduced the file size of Interlude.exe a little
- Fixed a note colors bug when loading the first chart
- Fixed level select screen not updating when you rename a collection
- Fixed level select groups taking 2 frames to collapse thanks to @9382

0.7.15.3
====

Minor patch amidst the code organisation changes (it's keeping me on my toes)

# Improvements in this update
- Fixed a bug when selecting charts in a playlist while viewing said playlist in Collections view
- Added a little www.yavsrg.net watermark to the bottom left of score screens! Maybe I can reel some new people in that get curious when they see your score posts

0.7.15.2
====

I've messed around with how I organise Interlude's codebase again - Everything is now in a YAVSRG monorepo  
Hopefully this is yet another shift towards a convenient workflow for both me and anyone else interested in contributing to the codebase

This release will be the last one listed on the Interlude repo so your existing clients can correctly update to future versions

# Solitary bug fix in this release
- Fixed a race condition in the preview image caching of Imports > Noteskins

0.7.15.1
====

Feature update 2 of a few in a short span of time!

# Bug fixes
- Fixed a bug when seeking to before the start of audio files in replay and practice mode
- Fixed a minor graphical glitch in score screen graph (hangover from 0.7.15's changes)
- Fixed some errors in chart loading under rare conditions
- Fixed scoreboard and leaderboard not updating until a chart fully and successfully loads
- Fixed visual offset not being factored into input overlay on replay mode

# New replay mode features
- You can pause replays (with space)
- You can speed up and slow down replays to get a better look (with the normal rate changing keybinds)
- New hit overlay in replay mode that highlights what judgements and ms deviations every input was awarded
  Particularly useful for reading off release timings even on weird skins
- Playfield dimming effect is now an adjustable slider so you can get the right contrast for the overlay over your notes

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

0.7.14.1
====

Minor bug fixes and improvements update before I go home for Christmas
I will be organising myself over the holiday for hopefully many fast feature updates in early January :)

# Improvements
- Fixed behaviour of `:` in search boxes (was reserved for special behaviour I never added)
- options.json and config.json to protect them from system crashes (new system for storing data coming anyway)
- Small bit of audio no longer plays as songs switch when in endless mode
- Improved some animations on the main menu
- Improved the Interlude logo slightly, should be a little more aesthetically pleasing

# Endless mode
- Some small improvements to pattern analysis have been made. **You will have to re-run "Cache patterns" from Options > Advanced as this update wipes the existing cache**
- SV files are recommended if you are on an SV file, and not if you are not

# Offset changer
- Pressing CTRL+O while ingame is back, and brings up a manual offset picker. If you use Automatic Offset, you can still make manual changes with this

0.7.14
====

# New features
- Proof-of-concept endless mode! For all your INFINITE GAMING needs

# Improvements
- Selected mods (and if the pacemaker is on) are saved when you close and open the game
- Suggestions engine is drastically improved
- Level select search box works on chart subtitles again
- Some more codebase improvements

0.7.13.2
====

# New features
- Changed how noteskin textures are rendered/sampled to reduce jagged edges on certain monitors
  It is on by default for your noteskins, but if you use bars/don't like this, it can be turned off for a crisper look
- You can now set custom column spacings, per column, per keymode via Options > Noteskins > Playfield

# Improvements
- Various codebase improvements
- Some performance improvements on information dense screens, particularly on level select (I now get ~680fps up from ~450)
- Density timeline on previews, practice mode and replays has been improved with some help from @9382
- Table suggestions screen has had some small improvements but still a WIP

# Bug fixes
- Fixed a bug when seeking to negative audio timestamps
- Fixed a bug where noteskin previews would sometimes show the top pixels of the preview at the bottom

0.7.13.1
====

This update fixes a crash on startup

If you are one of the lucky users to experience these crashes on startup, you will not be able to automatically update ingame, so instead you must:  

- Download this update manually
- Replace Interlude.exe with the new version
- Now launch it and you should be good

This patch also has some engine changes that I'm halfway through and had to cut short to fix the crash

0.7.13
====

Some new stuff for you!

# Features
- Hover over your accuracy in score screen to see it to 4 decimal places, instead of the usual 2
- You can now suggest charts for Crescent by right clicking on them in level select (there is a menu option)
- You can view suggested charts for Crescent under "Manage table" on table view (this UI is still a work in progress)
- Non-square receptors are now supported in noteskins for players who prefer to use them more like a key overlay

# Changes and improvements
- Users are sorted by time last seen in online menus
- Some fixes to localisation
- Fixed a bug preventing the game from launching via shortcuts and windows search
- Game should crash more gracefully if auto-restart fails

More things coming soon

0.7.12.1
====

Bug fix update  
Remember that if something doesn't look right or isn't working, I will only know if you tell me in the Discord!

# Fixes in this release
- Fixed a crash when hitting the 'random chart' key with no charts visible
- Fixed background dim only being 254 opacity instead of 255
- Fixed some performance issues during gameplay, resulting in over 60% more FPS on my machine
- Fixed BPM and chart duration not updating when changing rates
- Fixed typo in context menu key name
- Fixed alphabetical ordering of level select groups putting all lowercase names last
- Improved an issue in Smart frame limit on fullscreen where some frames were rendered twice
- Probably more that I've forgotten about

# Other changes
- Watching a replay now takes you to the score screen afterwards
- Added a `timescale` console command for testing animations

0.7.12
====

Thank you all for the support as Interlude launches its open beta!
Here's some new stuff based on feedback I got this week

# New features
- Ingame osu!mania beatmap downloads are back and improved, thanks to NeriNyan's API
- Under system settings you can pick a video mode for your monitor

# Bug fixes
- Fixed a crash related to viewing online leaderboards
- Fixed an error when importing stepmania files with no notes in them
- Fixed a visual bug in screen transitions, thanks to @9382
- Fixed some audio bugs when switching to another chart using the same audio file, thanks to @9382
- Fixed level select wheel over-scrolling if you queue up several actions at once, thanks to @9382
- Fixed a minor bug in online score saving
- Fixed a minor visual bug in level select

I'm also part way through massively cleaning up and refactoring code YAVSRG-wide so there will be some actual consistency in the code style, and contributing will be easier

0.7.11.1
====

A patch with a few bug fixes just in time for releasing the game trailer

# New feature
- Under 'Advanced' settings you can now update your cache of patterns
  This is so you can try the pattern analysis features like searching or grouping by pattern
  
# Bug fixes
- Fixed a crash related to the noteskin editor
- Fixed a bug where noteskin previews didn't update correctly
- Fixed the LN stuck in preview glitch
- Fixed a race-condition related exploit in audio loading
- Fixed clicking on a grouping/sorting dropdown option also clicking a chart immediately after

Ingame osu! downloads are currently down - Page on imports screen now points you to some manual mirror sites
It will be back soon :)

0.7.11
====

# New features
- NEW ingame overlay to find/search for players and view their profiles
	- Can be found in the top-right dropdown, hotkey is `F9` by default
	- In this overlay you can also change your name color and manage your friends list
- Added a prompt when dragging a Songs folder onto Interlude, to check if you want to link it rather than plain import
- Links in the ingame wiki are underlined

# Bug fixes
- Fixed transparency of noteskin previews on Import menu
- Fixed a bug causing the game to shut down twice, making it hang for a bit before closing

Also updated the wiki with some new pages :)

0.7.10.1
====

# New stuff
- Ingame wiki has had a major makeover
- Ingame wiki now uses the new site wiki, which has been updated a little
- The 'WIP' texture editor pages when editing noteskins have been fleshed out, allowing you to rotate, flip, add and remove individual textures
- The button to update the game has been improved, and can restart the game on successful update
- Fixed bug in screenshots after the file size fix of 0.7.9.2

Also check the site out, I've updated it to look a bit fancier

0.7.10
====

Noteskins update!

# New stuff
- Hit Ctrl+E to quick-open the noteskin editing menu
- Noteskin editing menu has had a major rework, including:
	- Editing playfield screen position, column spacing, etc
	- Editing animation speeds and settings
	- Editing note rotation values per column
	- Better tooltip explanations and previewing overall
- Score screen mean/SD only includes taps, not releases. Tooltip information shows the values for releases
- osu! Skin conversion now has some UI instead of just running (and failing) silently - More to come
- Some other skinning stuff in the works

# Fixes and improvements
- Fixed an unreported bug where hold note tails don't rotate with UseHoldTailTexture turned off
- Text entries in menus have a box around them so you can see them even when empty
- Various minor UI improvements

0.7.9.2
====

It's been a little while without an update, I've been looking into even more ways to improve how frames look on your monitor

# Even more engine improvements
- Reduced the file size of screenshots
- 'Smart' frame cap should look and work even better when on Windows (or running in WINE)
- Unlimited frame cap is Still the exact same as ever for people who prefer it
- Removed some debug messages

If you are running Interlude on your non-primary monitor you may see some issues in the new Smart frame cap, if so please report them in the discord

0.7.9.1
====

# More engine improvements
- Added compensation for 1 frame of visual delay on 'Smart' cap mode
- You can override the intended framerate of 'Smart' cap by editing config.json
- On 'Smart' cap, VSync will disable itself if the game detects it is not maintaining the intended framerate
- When VSync is off, a CPU timing strategy will be used instead, I STRONGLY recommend leaving VSync settings as default if they work for you

0.7.9
====

Major engine and performance update

# Smart framerate cap
Introduced a new 'Smart' frame limit mode, replacing the old numerical caps.  
Likely to be DRASTICALLY better for both your GPU and your gameplay experience :)  
'Unlimited' frame cap is exactly the same as before, for you to compare the difference and see what works best for you

# Performance tools
You can check performance stats ingame by pressing CTRL+ALT+SHIFT+F3  
CTRL+ALT+SHIFT+F4 hides the debug performance bars that otherwise will lower your FPS

# Smoother chart loading
Switching charts should now be smooth (no lag spike) - Even on the most slow of hard drives  
This sounds small but was a massive engine undertaking, so keep an eye out for glitches and report any if you see them

# Other fixes/changes
- Imported osu! scores now have an icon to indicate they were imported
- Level select displayed LN % is affected by selected mods
- Fixed absolutely countless minor engine bugs while redoing how charts load

0.7.8.3
====

The rare triple release in one day!

# Fixes in this patch
- Finally found and fixed the bug everyone was having with osu! score imports. **Should** now be working for everyone that experienced issues
- Right-click to clear search boxes (applies everywhere, not just level select)
- Fixed that awkward second and a half after searching where level select is fast scrolling to bring the results into view

0.7.8.2
====

# Various bug fixes and small feature requests
- You can right click to leave chart preview
- You can now hold left click to scrobble through a chart when previewing
- Improved stability of logging when the game crashes
- Improved game's crash recovery in many situations where the process exits suddenly
- Engine now technically supports negative hold trim values in noteskins
- Improved performance of osu! score import
- Score screen graphs correctly reflect final state in the last line
- 'Ratings' tab on table stats now shows top 100, with visual improvements
- Fixed bug where wrong chart names displayed in 'Ratings' tab on table stats

0.7.8.1
====

# Technical & bug fix update
- Logging now goes into a separate file per day
- Added more logging around osu! score imports to diagnose some errors
- 'Vanishing Notes' is now enabled by default for new users
- BPM meter now correctly scales with rate

0.7.8
====

# Import your osu! scores
If you have linked and imported your osu! songs library via the Import menu, there is now an option to import your scores too!  
Click 'Manage' and you will see a button for it.  
As with all new features, if you notice any bugs or errors please report them to me via Discord or GitHub.

# Other improvements
- When selecting noteskins, they now have an icon preview
- New HUD features: Rate meter and BPM meter (both requested by Lifly)
- Fixed visual bug in preview as it scrolls into view on options menus
- osu! rulesets have more accurate LN simulation thanks to some experiments while adding score importing

More noteskin UI improvements coming soon, but I ran out of time tonight and wanted to release what I had

0.7.7
====

Some new online features!

# Compare against your friends
In the Stats screen, under tables, you can now pick a friend to compare your table scores with.  
Add friends to your list via the Discord bot (UI to do this ingame coming soon)

# Claim a rank on table leaderboards
If you set a new table score you will get a position on the table's leaderboard, also accessible from the Stats screen.  
Hopefully this will encourage some of you to outfarm me to help find what needs adjusting

0.7.6
====

Many improvements and bug fixes in different places

# Bug fixes:
- Some broken background images/audio files in Crescent have been fixed
- Fixed the game logo disappearing if you click "Play" or any other screen-changing button too fast
- Fixed a bug in netcode when exiting the game
- Fixed some things not saving properly if you click the X on the black box to close the game

# Improvements
- Various minor things about the UI have been improved (icons, text wording, etc)

# New features:
- Personal best tracking has been rewritten to store ALL of your "best" info, rather than just your best across all rates, and your highest rate
- Added 'Patterns' grouping mode to level select - It's quite basic at the moment but does somewhat accurately categorise your charts
- Added 'Breakdown' tab when looking at table stats, it shows what scores contribute to your table rating

osu! and Etterna rulesets have been modified to hopefully make them more accurate to those games

Go to Imports > Rulesets to install the updates and see what you think

Your personal bests data will be wiped by the update, use the `fix_personal_bests` console command to recalculate them from your scores (takes about 30 seconds)

0.7.5.1
====

Fixes a crash on the Stats screen when you have no table selected

0.7.5
====

UI improvement update

# Bug fixes:
- Your leaderboard score now gets replaced if you upload a better score
- Can view leaderboards even without the intended ruleset selected

# Improvements:
- Score screen looks a little bit better :)

# New features:
- Added a 'Stats' screen, displaying some stats that have been tracked since 0.6.21
- Stats screen shows breakdown of your table grades if available (still under construction)

More stats stuff will come, hopefully it provides a motive for you to farm the table and therefore report balance issues

Also, nearly every chart in Crescent now has a leaderboard available

0.7.4
====

Some online features just for you

# New features
- If you are logged in, scores you set are automatically submitted to the server and go on your profile
- Some limited charts now have leaderboards (again requires a logged-in account)
- You can set your profile color via the discord bot, this shows up in multiplayer chat

More leaderboards for table charts will roll out as long as no problems come up

0.7.3
====

THE PATTERN ANALYSIS UPDATE  
Well, at least a good proof of concept

Upon updating your game will take a minute to cache all the patterns in every chart you have, for speedy searching!  

# New features:
- Random chart suggestions is much better at picking out similar charts for you, I recommend trying it out
- You can filter for patterns in the search box (try `pattern=jumpstream`)

# Bug fixes:
- Rolls in 4k charts should be detected as "streams" much less often
- Random chart suggestions will no longer suggest things it's already suggested before

# Improvements:
- Chart info mode when pressing Q looks more consistent with the scoreboard

These features need feedback on how they should work best, so let me know what patterns you'd like to be able to search for,
 and, if you'd like a grouping mode that groups charts by pattern, let me know what categories you'd like

0.7.2
====

I've updated some internals of the game - In particular "hashes", unique ids that can be calculated for each chart, have been changed  
This update will migrate your scores, songs, collections, etc to the new hashing system

The server will mark your client as out of date until you have updated

As a bonus any bugged osu! files you have scores on will be repaired and you will keep your scores :)

# Other bug fixes
- Score timestamps no longer display wrong when loaded from disk vs set while the game is open
- Pack imports may be significantly faster for people with slow drives
- Table level select mode should load/search significantly faster

0.7.1.2
====

Found a bug in osu! conversions, recently introduced by accident, that produces bugged LNs
If you converted your osu! songs folder in the last couple of weeks I would delete the imported charts and reconvert them
If you converted your osu! songs folder a long time ago you shouldn't see any issues and the bug is now fixed

# Other fixes
- You should now be able to download table charts right after installing a new table
- Sorting modes like 'Grade achieved' no longer show charts multiple times if you have duplicates in different folders

0.7.1.1
====

Turns out the frame limiter still had 2 more bugs in it  
They have been fixed and now the frame limiter is indeed better than ever

Also, added a description to the Crescent table on the imports screen and took away the WIP sign

0.7.1
====

Mostly engine improvements

# Improvements
- Stepmania pack import notification now displays when the import finishes, not when it starts
- Added notification when a recache is in progress to help confused users
- Table import UI is now a bit better in form and function
- Fixed a bug in frame limiters that caused stutters
- New frame limiter option for 720fps
- New experimental setting to override fullscreen refresh rates (for Lifly)

0.7.0
====

Ooo new major version!
The major version is celebrate adding something the game has lacked for a while :)

# New features
- Something sounds different..
- Import screen's table tab now downloads table charts via YAVSRG's chart mirror system. Still experimental but feel free to try it out  
  It will become something prettier in a couple of weeks

# Improvements and fixes
- Screenshot callouts now stay on the screen for 5 seconds instead of 2
- Fixed minor graphical issues with LNs (not the bug on previews though that still exists)
- Fixed bug where charts imported from StepMania packs didn't track which pack they came from internally
- Some UI refactors and consistency improvements (see if you can spot the differences since last patch)

0.6.25.2
====

Oops, I left a 1-liner bug in causing hold tails to render wrong on arrow skins  

This is a hotfix :)

0.6.25.1
====

Bug fix/patch update

# What's fixed
- Bugs with SVs with 'Vanishing notes' setting turned on
- LNs now behave correctly with 'Vanishing notes' setting turned on
- Fix issue where importing non-zipped folders will move the assets of imported charts
- Fix issue where BPM points are placed incorrectly when the BPM changes mid-beat in .sm files
- Stepmania converter now support stops!

0.6.25
====

Hope you're having a nice summer, I know I have been  
Here's some new stuff I added in between weekend excursions

# New cache system
- Implemented a new organisation system for the cache and Songs folder  
  YOUR CACHE WILL BE BROKEN WHEN YOU UPDATE, AND A RECACHE WILL BE NEEDED
  A RECACHE WILL MIGRATE YOUR EXISTING CHARTS TO THE NEW FOLDER STRUCTURE
  THE RECACHE WILL RUN AUTOMATICALLY WHEN YOU UPDATE AND OPEN THE GAME
  If that fails, you can still rerun it from Options > Debug > Rebuild cache
- New cache system will simplify many things in the future, like multiplayer downloads
  
# New features
- Thanks to the new cache, you can now group by Date added  
  This is the time when the chart was downloaded/converted/imported to Interlude

# Bug fixes
- Judgement meter, hit meter, early/late meter should now fade at the correct speed even on rates
- Fixed some Stepmania files converting without audio, amongst a couple of other bugs with chart conversions
- Fixed audio bug when seeking to start of song in practice mode

# Other improvements
- Applying settings presets has a confirmation page to fool-proof accidentally messing yourself up
- ^ The buttons are also further apart for the same reason
- Practice mode starts at beginning of song, not where it was playing when you enter the screen
- Converted Stepmania files get difficulty names like "4K Challenge 21" instead of "Dance_Single 21"
- Online signup page mentions the 18+ age requirement of accounts
- Some new menu splashes

0.6.24.1
====

# Some quick fixes
- Automatic countdown in multiplayer now works when people are spectating
- Fixed a bug where you would be kicked for notifying the server that you don't have a chart multiple times
- Lobby hosts can now select mods in multiplayer
- Spectator time delay has been reduced from a worst case of 6 seconds to a worst case of 2 seconds
- Some new menu splashes

0.6.24
====

Midweek release

# Changes
- Removed Life meter and HP from the game entirely. It was a dead-end for features and I didn't like maintaining it
- Score screen line graph is now colored according to the lamp you were on at the time
- Some bugs in the pattern analyser have been fixed or improved

0.6.23
====

# Spectating in multiplayer
- You can now ready up as a spectator rather than a player to watch other people play
- You can start spectating mid round, either if you quit out as a player or if you join the lobby midway through a round
- While it should be stable it is a bit janky (the audio pauses and resumes while spectating buffers sometimes)

# The Interlude.exe icon is back
Finally...

# Press Q to see patterns
- I moved the pattern analyser from a tooltip to text that replaces the scoreboard
- Press Q to toggle this; Better UI and better pattern data coming soon

0.6.22
====

# New features
- The options menu has been reorganised a little bit
  - Let me know what you think, hopefully you have to dig through fewer nested menus to find what you need now
- 'Advanced' settings is the home of opt-in experimental features
- Notification for taking a screenshot has a button to open the screenshots folder (requested by Wagg)
- New setting for hit meter to turn off the scaling factor for LN releases (requested by Lifly)
- You can change your ruleset while in multiplayer lobbies
- You can seek while watching replays to skip to where you want

### Pattern analyser
It's still a work-in-progress, but I've simplified its output to be presentable to users  
Press the tooltip hotkey (/ by default) while hovering over the difficulty rating to see this info  
With your feedback, it will become even more useful
  
# Improvements
- Mod select menu looks a little less poo
- Noteskin select menu is different, let me know what you think (I have more changes planned but am under time constraints)
- The 'chart suggestions' feature is now an experimental feature you opt into in the 'Advanced' options menu
  When you press the random key, it now just gives you an actually random chart by default

# Bug fixes
- Arrow key navigation in some menus is the most glitchless it's ever been
- Fixed several crashes in the import screen
- Fixed some crashes in the score screen graph
- Fixed a crash in multiplayer if you quit out instantly after the round begins
- Fixed a bug where the host could select mods just for them in multiplayer

0.6.21
====

A few fixes and improvements

# New features
- The 'Graph Settings' button does something after all this time
  You can now set a mode for a line graph to render, by default it shows your combo progression over time
- Game now tracks some stats like number of notes hit and your total playtime
  This will become a screen you can look at eventually, for now it is hidden away in stats.json (and controls the session time display on the score screen)
  
# Bug fixes
- Options menu buttons will not select themselves just because the mouse is over them, only when you move the mouse yourself
- Scrolling in containers with the arrow keys is no longer janky
- Scrolling in containers with the mouse wheel is no longer janky

I've also been improving the secret pattern analyser and some of its reports will be showing in level select soon
You can currently see the output by enabling Printerlude and typing `patterns` in the console

0.6.20
====

More things you asked for

# 'Vanishing notes' mode
LNs should also pass the receptors if not hit, and snap into the receptors when hit. They don't at the moment, this is a BUG
Unfortunately my render code is very old and delicate and I will need a big brain to fix this

I did however fix a bug with previews - All notes now look like they are being hit perfectly on previews

# Other bug fixes, improvements
- From now on, any newly cached charts will not break if the Interlude folder is moved somewhere else
  Run Debug > Rebuild cache in the options menu to apply this fix to your existing charts
- Optimised the level select screen. For very big open folders (10,000+) charts you should see double the framerate
- Optimised the 'What's new' screen. As changelogs piled up the screen was starting to run at about 30fps

# New and improved: Progress meter
Turns out peppy had it right all along, the best way to display chart progress is the familiar pie chart
The progress meter is now a pie chart, you can reposition it, change the size and colors, etc like the rest of the HUD

!! Existing users: You should click the 'Reset' button on the positioning for your Progress Meter as it will be using the old position of the falling dot

0.6.19
====

Some requested features to make the engine feel a little more familiar (mostly for 4k players)
I won't go into too much detail because Discord has a 2000 character limit

# 'Vanishing notes' setting
Go to Debug > Check this setting to make notes render like they do in Stepmania/similar engines
Hit notes will vanish and unhit notes will scroll past the receptors until they are hit or count as missed

4k players who play other games and have their reading directly tied to this visual cue, this may make you feel more at home
This is not enabled by default, you need to enable it!

# 'Automatic sync' setting
Go to Debug > Check this setting to enable automatic sync
Local offset will be adjusted whenever you finish/restart a chart
This means you can retry the intro of a chart 1 or 2 times to sync it
For manual syncing tools, visit Practice mode

# 'Prioritise low judges' setting
Gameplay > HUD > Judgement Meter
When this setting is OFF (default, and this is new behaviour), the judgement meter always shows the most recent judge
When this setting is ON, the judgement meter shows the worst judgement in the last 500ms (or however long the fade time is set)

You may find this less confusing and more familiar, even though it is objectively less useful IMO

# Other fixes
- You can make the bars thinner on hit meter, the minimum has changed from 5 pixels to 1 pixel

0.6.18
====

A couple of bug fixes + progress towards table community workflow

If you have an Interlude account, there is a bot in the Discord that lets you search for songs and charts
This same database will power other things like getting a chart you don't have in multiplayer, and installing missing charts for tables

I've mostly been working on that but here are some changes to the client too

# Fixes and improvements
- Trying to load a chart file that cannot be found on disk now shows a visible error message
- Added a 'Get more noteskins' button to the noteskins options menu, redirecting to the import screen
- You can hit enter while previewing a song to play it (you don't have to close the preview first)
- You can change the volume on options menus and while previewing a song
- Added a 'None' grouping method that shows all charts under one group
- Fixed osu! songs search loading when the game starts up instead of when the page is visited for the first time
- You can click on scores in multiplayer chat to see a score screen for them (let me know if you spot any bugs)

# Tables

Lots of things have changed about tables internally

- There is no longer a console command to allow editing tables from in-game and this feature has been removed entirely
- There is now a tab in the Imports screen for installing them (currently just Crescent, the 4k table, is available)
- Click to install tables
- Click again to attempt to install missing charts from various sources, using the aforementioned database (WIP and experimental)
- Hidden top secret feature: Right click on a table level in level select to see your average accuracy over all charts in that level

0.6.17
====

Something to tide you over while I work on server infrastructure

Accounts are no longer temporary and now is the time to grab your desired username
Early users get the permanent "early tester" badge which will show up on profiles later

# Other changes
- Some small UI adjustments (buttons now have yellow text on hover)
- The osu beatmap search has been improved with some new ways to sort and filter charts

As always please report bugs or anything that doesn't seem right quickly upon discovering it
(and thank you to those that are doing so already :)))

0.6.16
====

Multiplayer technical leaps

I'm testing out the new account registration system 
You can now link an account to your Discord account and that account is how you log into Interlude

**Please note: ** Accounts are currently temporary and get wiped whenever the server restarts
No data is held against them other than your Discord ID so try and break it/find bugs before I start storing data for real

You have a couple weeks cause I'm going on holiday

## Features
- All online features (multiplayer lobbies) use end-to-end encryption
- Placeholder login where you 'claim' a name replaced with log in via discord

## Other fixes/improvements
- The Interlude.exe icon might be back, I'm not really sure until it goes through the build pipeline
- I've rewritten some major parts of Prelude, hence some time between releases to see if I spot any bugs
  You may see some improvements in background image detection for .sm files and various metadata conversion fixes
- Fixed a rare crash when interacting with osu! song search
- osu! downloads use a different mirror (perhaps multiple mirror options coming soon)
- Some new menu splash messages

0.6.15.3
====

Features and changes that came directly from playtesters in the Discord
Thank you all for playing and giving feedback

## Bug fixes
- Fixed bug in osu! and Wife hit detection reported by Eliminate
  Now you will cbrush just as Mina and Peppy intended
  
## Improvements
- Startup sound effect is no longer DEAFENING as suggested by Jude

## New features
- Added "No holds" mod as suggested by Wagg
  This is not ranked and will not count towards pbs
- Added Early/Late meter as suggested by lemonmaster

0.6.15.2
====

## Improvements
- Gameplay setting presets now have hotkeys for switching them quickly (for you multi-keymode mains who use different settings for each)
- Record improvement indicators on score screen now display how much you improved your record by

## Features
- The fabled Judgement Meter HUD item has made a return after many years being commented out
  Currently it displays all judgements (there is no way to turn off Marvellous/300g from being displayed)
  Hiding the "perfect" judgements will be added when the early/late meter is added which will also be soon

0.6.15.1
====

More fast releases based on bug reports and feature requests
There will be more stuff later today

## Bug fixes
- Personal best recalculation algorithm was counting NoSV scores towards personal bests when it shouldn't
- Changing noteskin didn't save for next time you open the game
- Fixed some hotkeys misbehaving on practice mode screen

## Improvements
- Search box (and some other things) now highlight in yellow when they are selected
- By request, practice mode calibration for scroll speed and hitposition is now possible even with audio turned on (even though I do not recommend this)

## Features
- Added gameplay setting presets
  These can be loaded and saved via Options > Gameplay
  Stores your scroll speed, hit position, upscroll setting, visual offset, noteskin and lane cover settings
  
  Try it and tell me what you think

0.6.15
====

Fast release on request of Wagg

## Bug fixes
- Fixed crashes when previewing charts while there is no selected chart to preview
- Fixed crashes when exporting a noteskin folder that you've moved with the game still open
- Fixed some issues with noteskin menu not refreshing after making a change

## Changes
- The score screen looks different... :o
  This is still a work in progress but I'm releasing what I have at the moment for feedback

0.6.14
====

## New stuff
- Rulesets are no longer part of themes. Instead, rulesets are saved in the new Rulesets folder.
  The game starts with only SC (J4) installed. You can add your favourite rulesets by going to Imports > Rulesets and using the
- New ingame downloader for rulesets
  This is where updates for rulesets will be distributed from now on (although it could be prettier)
  - osu! rulesets have been updated so that the lamp names are better and more useful
- There's no longer a reason to create your own theme (unless you want to try it out for fun) so I've relegated themes to the debug menu, and noteskin management is done from the Gameplay section of options
- Sliders in the options menus should be less annoying to use
  - Updated the precision at which most sliders naturally adjust by to sensible values
  - Hovering over the slider and scrolling with the mouse wheel allows sane adjustments
  - Tapping the arrow keys with the slider selected allows sane adjustments
- When adjusting HUD components in the options menu, you can now *hold down* the arrow keys to move things around
- Multiplayer is open to a slightly wider audience because you don't need to enable console commands to use it any more
- **PRACTICE MODE**
  This is a new screen available from level select (see the new target icon, hotkey is V)
  PRACTICE MODE allows you to play the current chart from a point of your choosing, for practice. There are hotkeys for pausing and retrying from the same point.
  While paused, you can receive suggestions on what scroll speed, hit position, visual offset or audio offset to use to compensate for how early or late you currently hit when you practice.
  I encourage you to try using this tool to sort out synchronisation issues and provide any feedback you might have!
  
  After a first round of testing this tool will also be available from multiplayer lobbies so that you can check your synchronisation on those pesky charts you've never played before
  
Also fixed a bug in the display of miss counts on the score screen

I hope you enjoy :)

0.6.13
====

Stuff

## Fixes
- Input overlay when watching replay now displays correctly on rates other than 1.0x
- Can now seek to before the audio file begins when previewing a chart

## Improvements
- Better warnings in the log when things go wrong with mounted imports and noteskins
- osu! downloads via Import screen look better and should work on more charts thanks to Nerinyan.moe
- Noteskin downloads via Import screen look better

## Changes
- "Gameplay widgets" have been removed from themes. Instead you can configure these globally via Gameplay > Configure HUD and these settings are saved in the Data folder
  To preserve your existing data, copy it from your theme to the `Data/HUD` folder
- Removed grade textures from rulesets (this only affects SC) - So that rulesets can also be dropped from themes in the future

Also the entire YAVSRG project uses a polyrepo now which I'm finding interesting

0.6.12
====

More UX improvements
Special thanks to Eliminate for playtesting the game while sat next to me, I got a TON of useful info out of that

# New features
- You can now choose your windowed resolution properly ingame via the System settings
- While watching a replay, can now turn on Input Overlay to see the raw inputs
- Can now change song rate while previewing a chart

## Improvements
- The songs folder now has a "HOW_TO_ADD_SONGS.txt" to help confused users
- Pressing ESC now closes the comment UI/open dropdowns instead of leaving the level select screen
- Extracted themes/noteskins now have a real folder name instead of nonsense
- Improved performance of level select screen when other menus are overlaid on it (preview should lag less when 10000 charts are visible)
- Logo now has a "breathing" effect

## Fixes
- Fixed checkbox options having their icons the wrong way round
- Fixed chart preview not including some leadin time before the first note
- Fixed audio position skipping around when changing rates (now it should be seamless)
- Fixed mean/standard deviation on score screen treating misses as +180ms
- Fixed linking to another game's song library not instantly importing it
- Fixed some missing hotkeys from tooltips
- Fixed dropdowns turning blank when the window is resized

0.6.11
====

UI and UX improvement update!

## Bug fixes:
- Fixed issue with multiplayer chat not scrolling properly
- Fixed rare crash when previewing certain charts

## UI changes and improvements:
- Tooltips now look fancier (and I've updated them with better information)
- Cursor turns white if tooltip information is available
- Options menu has been redesigned a little (still more to do)
	- Gameplay binds are now under Gameplay
	- Hotkey binds are now under System
	- Keybinds menu has been removed
- Lots of UI changes, mainly color scheme stuff (let me know what you think)
	- Notifications
	- Mod select
	- Toolbar buttons
- Most of the UI changes are geared at making it easy for players to naturally discover/stumble upon the features they need
	
## Feature re-releases
The "Random chart" and "Comments" features have existed for a little while, but noone knows they exist
This is my fault for not making it easy to discover the features, so now there are visible buttons on the level select screen
Consider them re-released

Comments = You can write arbitrary notes on your charts and search through them
Random chart = Not actually random, it uses the first draft of a suggestions algorithm to recommend you a similar chart. Try it out! I need feedback

I won't say more than that as hopefully the new UI stuff ingame lets you find out how these work fully on your own, we'll see

## New stuff
- You can preview charts while in a lobby
- You can add charts to collections while in a lobby

0.6.10.4
====

Fixed not being able to connect to the online server  
I commented something out and forgot to put it back

0.6.10.3
====

- Minor bug fixes
- When login fails, the server doesn't just kick you with a message any more
- Idle connections time out after 2 minutes, fixing "Username is already taken" bug

0.6.10.2
====

Here's some more multiplayer features!

### Features
- When round ends, you can now see lamp and grade scored by you and other players
- Pressing "Start round" now has a 5 second countdown so it's less abrupt
- Added lobby setting: Host rotation (can be turned on/off)
  Automatically passes host to the next player whenever a round successfully completes
- Added lobby setting: Automatic countdown (can be turned on/off)
  The round countdown automatically begins if everyone is ready
  If someone is no longer ready, it automatically cancels
  
### Fixes
- Occasional issues with automatic login on game start
  
Host rotation is not reliable with big groups/strangers as someone can go AFK and never pick a song
Instead of adding more features to deal with this, there will later be a "Song queue" feature that anyone can add to so that host rotation doesn't need to exist

0.6.10.1
====

Multiplayer bug fix update

- You can now see invites to lobbies on the lobby list screen
  (notifications/alerts when you aren't on this screen are yet to come)
- Fixed like a good few dozen crash bugs
- Some small UI improvements while in the lobby screen (mostly indicators for missing a chart)
- Multiplayer results printed to the chat look a bit better
- Added a temporary display of live scores while you're playing
  This is enabled if Pacemaker is enabled in your theme (and uses its placement data)

0.6.10
====

First release of multiplayer!

In this version of the client there is now a secret way to
- Connect to an online server
- Start or join a multiplayer lobby
- Play songs together in it

This is heavily under development so expect some bugs and imperfect UI design

You can claim a username when you connect to the server but *THIS IS NOT AN ACCOUNT* and when you disconnect, the name becomes free to claim again.
Accounts are coming later.

The only data stored about you is:
- The username you choose when you connect
- State such as what lobby you are in
- The replay for the score you are currently setting in a lobby
This data is deleted when you disconnect from the server/exit the game.

0.6.9.2
====

Bug fix update for my beloved play testers

- Bug fix: Releasing a long note on osu! ruleset will now correctly break your combo
- Bug fix: Combo breaks other than from missing now correctly affect lamp on osu ruleset
- Bug fix: Dropped hold notes no longer only grey out when the tail is visible on the screen
- Some other adjustments to error logging and stuff

I normally make a rule not to mention things that are work in progress in these changelogs
So I'm not going to, but if you're in the discord you may know what is underway :)

0.6.9.1
====

A few goodies on the way

- Improvement: Changed max column width to 300 in the ingame noteskin editor (you can set arbitrary numbers by editing the json file)
- Improvement: Adjusted the design of the score screen a little bit
- Improvement: Added some new menu splash messages
- New feature: Seeking + density graph when previewing a chart
- New feature: Session timer on the score screen
- Bug fix: Issue when converting .osu files with notes in the wrong order

0.6.9
====

### Interlude now runs on .NET 7

.Net Core 3.1 is out of lifetime support and it was surprisingly easy to make the upgrade 

This shouldn't mean anything for you, but if anything no longer works as it should please report the bug on discord

Also, windows releases of the game are now an all-in-one that contains the .NET 7 runtime. 
This makes the game and future updates about a 80mb download instead of 15mb, but bandwith/disk space is cheap and getting people to install a framework is not

#### Other changes

- The 'update available' message now reminds you that you can install it from ingame
- New feature: Column spacing as a noteskin setting
- Some hidden code for a secret feature I'm working on :)

0.6.8.6
====

Yet Another Quality Of Life Update

- New feature: Inverse mod (coming soon: customising of the gap size)
- Bug fix: NoSV would always count as being applied on osu! converts (unranking your score)
- New feature: There is a clickable back button on options menus (alternative to pressing escape)
- New feature: You can export your noteskins as .isk files via the noteskins menu
- Bug fix: Sort by Technical rating is gone
- Improvement: The mod menu/pacemaker UI has been split
  And so it shall stay for the time being, so I've removed the WIP markers

0.6.8.5
====

Bug fix update, Yippee!

- Bug fix: A crash when you don't have any favourite rulesets
- Bug fix: Ruleset picker (the big one, not the quick switcher) looks better and is less glitchy
- Bug fix: Technical rating is shown and is confusing. It is no longer shown
- Bug fix: Clicking on sorting/grouping buttons was treated as also clicking on the chart below the button
- Minor feature: Press Shift+D to open the ruleset picker (the big one not the switcher) on level select and score screen

0.6.8.4
====

Small update grindset

- New feature: Sound effects
  More will come from this in the future but for now I've brought back a familiar sound when launching the game :)
- Bug fix: "Favourite" rulesets as picked from the options screen now stay selected even when changing themes

Other improvements to rulesets:

- Ruleset switcher on level select now shows a selector instead of just cycling rulesets
- Switcher now appears on score screen and is how you switch rulesets there too
- In both cases the hotkey is D by default (you can no longer use arrow keys on the score screen)

More improvements to the ruleset stuff coming soon I just wanted to put this out for the bug fix

0.6.8.3
====

Back on a bit of a roll with these patch releases

- Bug fix: Previews would get out of sync when changing audio rate
- Bug fix: You couldn't bind a hotkey to any of the navigation keys
- New feature: Judgement counts display
  This is turned off by default, go to Themes > Edit Theme > Gameplay config > Judgement counts > Enable to turn it on

0.6.8.2
====

Small update

- Winterlude time is over
- New feature: You can left-click and drag to scroll through level select
- Improved old feature: You can right-click and drag to fast-scroll without opening the menu for a chart (before, you had to right-click in the space to the left of the charts)

0.6.8.1
====

### Winterlude 2022

Small update before I'm away for Christmas

- Default bar and arrow noteskins now use DDR coloring, which they were always meant to have as the default
- Brought back the Winterlude logo effect :)

0.6.8
====

## What's a table?

A table = a ton of charts hand-sorted into levels/folders.  
You work through each folder, setting a certain standard of score, and each folder is a good-sized step to slowly push your skills and improve.

Interlude is getting tables because:

- It gives the game the closest thing to "official" content - There will be clear goals to achieve rather than just picking stuff to play every session
- It lets me collect some sweet sweet analytics data for difficulty calculation algorithms
- It provides useful progression advice to players while the aforementioned difficulty calculation algorithms are made

## Table stuff

Following on from 0.6.7's collections update, the "Table" library mode and buttons now do more stuff

- Clicking 'Manage Tables' displays a menu where you can select any installed tables if you have any (Check discord for initial release of a test table to try this out)

- Added some hidden stuff for creating and editing your own table. You can toggle it on via Printerlude if you're curious

- I'm in the process of setting up:
	- Some nice tooling for contributing to tables and publishing changelogs when they get updated
	- Some infrastructure for installing tables into your Interlude client from the 'Manage Tables' menu

0.6.7
====

Collections and quality-of-life update

## Collections

- Collections have been rewritten. As previously mentioned if anyone has collections they want to keep let me know in the discord.
  
- There are two kinds of collection: Folders and Playlists
  Folders are groups of charts like osu! 'collections'
  Playlists are ordered lists of charts including the gameplay modifiers and a specific rate for the chart
  
- Added library views: All, Collections and Table
  These are what gets shown on level select.
  
  'Table' view is a placeholder for more fleshed out table features
  'Collections' view lists all collections
  'All' is just the normal list of every chart you have

- You can quick-add and quick-remove charts from a 'selected' collection (more info is provided by the relevant menus)
  This is a lot quicker and more convenient than going through the right-click menus.

- Collections can be assigned various icons, and these are shown in level select. The display icons are a bit smaller and look better when you also have a comment on the chart.

## QOL

- Hotkeys can now be individually reset in the keybinds menu
- Hotkeys can now be reset all at once in the keybinds menu
  **I would recommend doing this as some bindings have been changed and will not automatically change when you update**

- Scoreboard looks a little different :)
  Also you can now focus it by pressing Z (by default)
  This lets you navigate it with the keyboard/press enter to view the screen for a score
  
- You can press . (by default) to open the context menu for charts (or for scores with the above scoreboard navigation feature)
  Equivalent to right-clicking.

- Added and updated a BUNCH of hotkeys around level select for ease of navigation with keyboard-only if that pleases you

- The hotkey for many level select buttons are shown when displaying the tooltip info for that button. The key to show these is still ? (by default)

- Fixed several bugs:
  - Etterna pack downloads should be up again (caused by expired certificate)
  - Scoreboard refreshes when selecting mods with "Current mods" filter active
  - Some others
  
- Various locale consistency fixes

0.6.6
====

- Fixed yet another bug in the auto updater
  It couldn't tell the difference between 0.6.5.1 and 0.6.5.2 so thought the game was up to date
  So I created this feature release (albeit small) that you can update to
  
- Added a changelog button to the main menu
  It reuses the ingame wiki feature to view the full Interlude changelog
  It turns yellow when an update is available
  
- Ingame wiki/changelog now has a button at the top to install updates
  You can also still update via the debug menu
  
- Added a Discord button the main menu. It links to the discord :)

0.6.5.2
====

- Removed "Goals" collection type, this is in advance of a new goals system in the future :)
  This will break your collections.json if you have a Goals collection (I'm certain nobody is)
  
  There are other collections changes coming up which may break collections
  When the time comes, if anyone really does have collections they want to preserve give me a shout in the discord
  
- You can now hold right-click to fast scroll on scrolling containers.
  This is to help with navigating Imports > Etterna packs and Options > Keybinds > Hotkeys

0.6.5.1
====

- Fixed bug in pacemakers targeting specific judgement counts
- "Save under-pace scores" setting now functions
- Fixed bug with auto-updater since 0.6.3 (you will have to manually install this version, sorry!)
  
To manually update an existing game, download the zip and extract the files over your Interlude folder.
When asked if you want to replace files, choose yes.

If you are on 0.6.2 or below the auto update should work fine, it was to do with the new build pipeline tech

0.6.5
====

## Stuff that's new

- This changelog is automatically posted to Discord :)
- Fixed some crash bugs
- Renamed Screencover to Lane Cover (this is what it's more commonly known as)
- Pacemaker stuff as explained below

### **Pacemaker features**

- Added a positionable pacemaker gameplay component (access via Themes > Edit Theme > Gameplay Config)
- You can now set and use per-ruleset pacemakers (e.g. If you are on Wife3 you can set a pacemaker for 93.5%)
- Pacemakers can either be an accuracy target or a lamp target
- When enabled, your pace compared to accuracy targets is shown with a flag (a bit like the crown in osu! lobbies)
- Lamp targets as shown as the amount of "lives" you have
  Getting the wrong judgement loses a life
  When you run out of lives, you missed out on the lamp!
- Special feature: Challenging scores!
  To challenge a score, right click it in the scoreboard and select "Challenge"
  This will make it a pacemaker as you play, and indicate if you are ahead or behind the existing score's pace
  Being ahead/behind is updated based on the accuracy you had at that point in the chart (not final accuracy of that score)
  
To access pacemaker settings you can either:
- Configure them as before under Options > Gameplay > Pacemaker
- Configure them under the Mods menu in level select

The pacemaker must be turned on by you when you want to use it (it is not always on)
This is to make it something you declare to yourself you're going to do right before playing (a bit like using Sudden Death in osu!)
I'm interested in what effect this has:
- Maybe it will make you play better and focus properly
- Maybe it will be used to signify the end of warming up
- Maybe it will never get used because everyone will forget to turn it on

0.6.4
====

New features around user settings!

- Under Themes > Edit Theme > Gameplay Config you can now edit the position and other settings about gameplay widgets
  - Includes accuracy meter, combo meter, hit meter and more
  - Includes tooltips explaining what things do
- Added a color picker for screen covers, allowing you to change the color from ingame
- Fixed several bugs with gameplay widgets that were discovered while putting these settings in

You can now do more theme stuff in-game (and see its effects quicker) instead of making manual edits in your theme folder

0.6.3.1
====

This release is a test of my automated publishing system :)

- Improved buttons on import screen mounts by moving them up slightly
- You may also notice the release zip just has an exe, locale file and audio dll. That is new!

0.6.3
====

⚗️ Experimenting with a more formal changelog and some automated systems for publishing releases

- Adjusted chart suggestion algorithm to give a little more variety in suggestions
- Quick-start-guide feature has been replaced with new Ingame Wiki feature
  - Ingame Wiki launched on first install
  - Ingame Wiki lets you navigate Interlude's wiki pages from within the game
- Fixed bug causing reloading noteskins/themes to not work correctly
- Fixed bug causing note explosions to be the wrong size
- New import screen
  - Downloads now show progress in the Import UI
  - Downloads can now be retried when failed
  - UI can be navigated using keyboard only (as well as mouse)
  - Downloads can be opened in browser
  - Noteskin previews are now cached and fade in as they load
- Fixed bug with import screen where drag and drop imports did not work
- Removed 'Tasks' menu - Background tasks have been rewritten
- Improved buttons for Graph Settings and Watch Replay on score screen
- Added a few new noteskins to the repository

0.6.2
====

- You can search for charts by length and difficulty (by typing l>60 in the search, for example)
- Bug fixes including imports not being broken
- Bug fixes including tabbing out of fullscreen not dividing by zero
- Improved comments, you can also search for them via the search bar too
- You may not have known that comments exist. The hotkey is Shift + Colon
- Pressing F2 will select a "random" chart. It actually uses a suggestion engine like a boss to recommend you something similar

0.6.1
====

- Fixed 2 crash bugs
- You can put comments on charts (feature still needs a bit of polish but does in fact work)
- There is now a dedicated screenshot key
- There is now a hotkey to reload themes/noteskins instantly

0.6.0
====

- Interlude now runs on my game engine (Percyqaz.Flux)
- Some UI things look a bit nicer
- Added chart previewing on level select
- All user reported issues should be fixed
- Overholding on Wife3
- EO pack downloads have a hotfix for the next couple of months
- Other stuff (it's been a while)

0.5.16
====

- Some bug fixes
- Major refactor to UI (no visible changes)
- Noteskin repository! Go to the imports screen to see the (at time of release) single noteskin that's available. Feel free to send me noteskins to be available there (I am also going to add some more soon)

0.5.15
====

- Hotkeys are now rebindable! See the Keybinds menu in Options
- Added a quick-retry hotkey (default Ctrl-R)
- Finally fixed skip button causing desync on certain mp3 files (many thanks to @semyon422)
- Basic osu! skin to Interlude noteskin converter is back
- Users can now create and manage their own tables, and display them in level select. This feature is very much a work in progress, with no proper user interface for managing tables just yet. If you are a table maintainer, contact me for a guide on how to get started!
- Many new level select features, including new navigation hotkeys and reversing sort/group order
- A couple of hidden noteskin tools built into Printerlude for common "how do I do X?" tasks I can now walk someone through
- A couple of bug fixes

0.5.11
====

- We have moved repo! 🥳 From percyqaz/YAVSRG to YAVSRG/Interlude
- Apart from that, just some bug fixes with big stuff on the way 🕶
- I use emoji in commit messages now 👽👽👽