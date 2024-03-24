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

