# What's this?

This is the rhythm game framework Interlude (in due time) runs off.
It implements the core elements of the game as a class library but no game engine features like rendering/playing sound/getting keyboard input.

The purpose of this framework is to provide a very easy set of tools for working with Interlude and the format it stores data in, however I have designed some aspects of it with general purpose rhythm game tools/reusability in mind.


What you see above right now is an old C# implementation but I am currently rewriting all of this in F# so that it is
- More organised, neat and concise
- Easier to verify correctness/debug
- Built for .NET Core as Interlude was previously built for .NET Framework making it harder to port to macOS and linux.

## Features (of the F# version)
- Parsing/reading of .sm, .osu and .yav file formats for charts
- Conversion between these chart formats and writing them to files
- Particular support for storyboard editing for .osu files
- A handful of filters/tools to edit Interlude charts to form a base for a chart editor
- Tools to parse Interlude scores and process score accuracy
- Chart difficulty calculation
- Chart caching and management of a songs folder
- Multi-threaded task management, mostly for batch converting packs to Interlude

## Coming soon
- Gameplay modifiers and colorizing
- Support for parsing .ssc + some others
- Web tools/protocol implementation for score servers
- A wiki full of examples on how to use Prelude to perform tasks such as editing an osu file or calculating Wife% for a score
