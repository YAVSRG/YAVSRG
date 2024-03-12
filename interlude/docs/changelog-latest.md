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

