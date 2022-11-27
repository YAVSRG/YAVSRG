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