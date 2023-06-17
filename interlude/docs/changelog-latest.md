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

