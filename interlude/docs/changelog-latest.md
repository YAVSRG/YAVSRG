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

