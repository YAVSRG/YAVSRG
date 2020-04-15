# What's this?

This is the rhythm game framework Interlude (in due time) runs off.
It implements the core elements of the game as a class library but no game engine features like rendering/playing sound/getting keyboard input.

The purpose of this framework is to provide a very easy set of tools for working with Interlude and the format it stores data in, however I have designed some aspects of it with general purpose rhythm game tools/reusability in mind.

I am currently in the middle of rewriting more of all of the YAVSRG project in F# so that it is
- More organised, neat and concise
- Easier to verify correctness/debug
- Built for .NET Core as Interlude was previously built for .NET Framework making it hard to port to macOS and linux.

## Features
- Reading, writing and conversion of .sm, .osu and .yav file formats for charts
- Basic support for storyboard editing for .osu and .osb files
- A handful of editing filters/tools to form a base for a chart editor
- Chart modifiers, note color schemes and difficulty calculation
- Score processing with HP, accuracy and difficulty performance metrics
- Chart caching and management of a songs folder
- Background task management, mostly for batch converting packs to Interlude

## Future Features
- A wiki full of examples on how to use Prelude to perform tasks such as editing an osu file or calculating Wife% for a score
- Support for more chart file formats
- Interlude -> Stepmania conversion and writing
- Management of data like settings, scores and themes
- Web tools/communication with score servers
