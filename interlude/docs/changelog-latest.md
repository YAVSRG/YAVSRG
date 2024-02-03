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

