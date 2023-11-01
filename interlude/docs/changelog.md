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