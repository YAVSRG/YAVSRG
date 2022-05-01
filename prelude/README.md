# YAVSRG :: Prelude

This is the rhythm game framework [Interlude](https://github.com/YAVSRG/Interlude) runs on.
It provides the core elements of the game as a class library but no game engine features like rendering/playing sound/getting keyboard input.
In the future, this will also provide the core elements needed for a full rhythm game chart editor.

The framework has been made separate from the game to:
- Provide a very easy set of tools for working with Interlude and the format it stores data in
- Reuse large parts of it as general purpose rhythm game tooling

## Features
- Reading, writing and conversion of .sm, .osu and .yav file formats for charts
- Basic support for storyboard editing for .osu and .osb files
- Basic support for parsing osu skin.ini files
- A handful of editing filters/tools to form a base for a chart editor
- Chart modifiers, note color schemes and difficulty calculation
- Score processing with HP, accuracy and difficulty metrics
- Chart caching and management of a songs folder
- Management of data like settings, scores and themes
- Background/concurrent task management, mostly for batch converting packsfrom other games to Interlude's format

## Future Features
- A wiki full of examples on how to use Prelude to perform tasks such as editing an osu file or calculating Wife% for a score
- Support for more chart file formats (.qua is top of the list)
- Any new Interlude-related features that it would be useful to do outside of Interlude itself
- Interlude -> Stepmania conversion and writing to file
- Web tools/communication with score servers
