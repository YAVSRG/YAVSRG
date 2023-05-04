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

Minor bug fixes
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

Some bug fixes
Major refactor to UI (no visible changes)
Noteskin repository! Go to the imports screen to see the (at time of release) single noteskin that's available. Feel free to send me noteskins to be available there (I am also going to add some more soon)

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