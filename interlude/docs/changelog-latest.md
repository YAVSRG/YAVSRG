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

