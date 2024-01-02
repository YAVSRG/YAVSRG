# YAVSRG Project Roadmap

Let's turn a new leaf and try this method of todo list tracking, since I seem to constantly gravitate to having a notepad open and just using that

Aim: All of my future action plans for Interlude should exist in one of three forms:
- A GitHub Issue on YAVSRG/YAVSRG (this repository)
- A commented `todo` inline in the source code for tasks that are too small or me-specific to be worth writing up in detail, but not worth forgetting
  All of these can be found by searching for the exact characters `// todo` in .fs and .fsx files
- In this README file, where I can take shorter form notes than GitHub issues

Discussion around these features normally takes place on Discord

Sections are ordered by rough "key goals" the project has

----

## 🎮 Gameplay features

### Multiplayer
Multiplayer is largely working and stable, allowing for spectating in lobbies while you wait and a few other nifty things

Here's my todo list:
- Support selecting mods
	- The host picks and everyone has to use the same mods at first
	- Later, everyone can freely pick mods (maybe as an option the host can opt-in for)	
- Possible bug to investigate: Go into a lobby while mods are selected and experience bugs
- Support opening practice mode while in a lobby so you can make sure it's synchronised
- Add a Song Queue
	This feature replaces the "Host rotation" setting that exists at the moment
	- Queued songs include the rate (and mods) at the time of selection
	- Host chooses if just the host or everyone can add to the queue
	- Host can add restrictions on what can be queued (keymodes, min and max length, min and max difficulty, number of songs queued by one person)
		- Selected filters automatically apply to level select when picking? Would be nifty
	- Host can veto queued songs
	- Host can reorder queued songs
	- Host can freely pick any song in the queue and put it at the top
	- Host can enable/disable a vote to skip songs
	- Queue automatically advances forward after a song is completed
	- Some kind of UI to see the history of the queue so the host can restore skipped or removed songs
- Support players playing the same song at different rates? Although I don't personally believe in this much
- Chat commands
	- Rolling dice (for tournaments)
	- Commands to interact with queue quickly instead of using the UI
	
### Spotlight search
A new capability of the options menu to let you search for settings

- Search results are selectable and take you straight to the relevant page and setting, highlighted
- Search results should also pick up key words and suggest wiki pages

### HUD and Playfield features
- Requested: Skinnable images for left and right of the "stage" like in osu!mania
- Requested: HUD component that displays information on the current chart, rate, difficulty, etc (different items toggle-able)
  Useful for streamers or people who forget what they are playing midway
  Also useful for endless mode when introducing the next song
- Requested: Skinnable judgement indicators
  This has taken a while because the list of existing judgements changes depending on your ruleset! This will need to be customisable per-ruleset or per family of similar rulesets
- Requested: More control over explosion animations
  - Control the exact milliseconds the animation lasts for
  - Toggle the fading effect for animated frame animations
  
### Endless mode
- Would be nifty if the game could preload the next song ahead of time and crossfade it seamlessly
- Endless mode needs a UI that gives you stronger control over the picks and what kind of session you want
- Support for playing a playlist in endless mode
  
### Various UI improvements
- When there is no page to go back to on the ingame wiki, "Back" should say "Close" and close the UI
- Score screen should have gadgets on it to add a song to collections, edit its note, whatnot
- Polish UI in replay mode with some extra tooltips and explainers
- Redesign practice mode UI with the new slideout components
- Add a hit and lamp graph to replay mode so you can browse to your mistakes
- Ingame single-chart download similar to osu!direct

#### New "chart selecting" UI and behaviour
- Charts should be selectable in level select (shift+click or [ and ])
- While any charts are selected, a hint indicates how many are selected and that you can perform batch actions
	- Delete the selected charts
	- Add the selected charts to a collection
	- Create a new folder or playlist with the selected charts
- UI to cycle between subtitle/difficulty name, source pack/folder, comment underneath each chart
	
#### New main menu
- Would be nice to have some kind of news feed/blog posts that show up on the main menu
- Add a credits button and page
- Add a jukebox feature that uses suggestion algorithm to pick next song

### Tables
- Finish off the UI for suggesting and adding to tables
- Allow downloading only a section of a table at a time (e.g. just levels 20 - 30)
- Regular cadence of table updates

### Goals
- Ingame goal tracking of some sort
- Interlocks with endless mode to give you suggestions that move towards your goals
- Goals can be targets of playing X amount of Y pattern every day
- Perhaps some automated daily or weekly goals
- Indicate what goals you've achieved

### Challenge your friends
Online server needs an ability to send a replay of a song to your friends like a message or something
The score can just be to brag
You can immediately go to that chart and play against their replay, and reply with your replay when you do

This captures this "hey you should play this" playstyle lots of people enjoy with their friends
	
## Developer/community features
Things to foster a nice gaming community around Interlude :)

- Contributor guide including some zero-assumed-knowledge guides to opening a PR
- Update site to showcase the new trailer
- News feed/blog post system so that new updates come with screenshots or additional reading on them
- Make Prelude available as a NuGet package (with name YAVSRG.Prelude) for developers looking to use its core libraries for cool projects (I do this sometimes)
- Guide to using the API if you wish

## Technical improvements
- Use SQLite to store various data (mostly scores)
  It's more reliable than JSON files directly to disk - This is in progress at percyqaz/Percyqaz.Json (which is becoming percyqaz/Percyqaz.Data)
- Discord rich presence! Library I found for this has an issue where it tries to reconnect every 10 seconds if discord is not running, lag spiking the game
- Requested: Non-pitch rates. Not a priority :)
- Maybe hitsounds or clap tracks at some point

### Interlude "Paths"
Register a URI scheme on your computer so that links like `interlude://settings/gameplay/scroll_speed` work  
Particularly useful for other things like `interlude://join-lobby/{lobby_id}`, `interlude://chart/{chart_id}`

## Prelude Features
Things that would be cool to have a library for even if I don't immediately use it in Interlude - Could be used in other rhythm game stuff I do

### Pattern generation
- Imagine "regex" for patterns (except it works both ways and can generate patterns too)
  If you can good job because I've had a crack at this a few times and it turns out trying to represent the 2d complexities of patterns with a 1d string isn't easy
  More research needed

----

There are more things to list as I remember them