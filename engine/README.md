# Percyqaz.Flux

This is my custom game engine built on top of [OpenTK](https://github.com/opentk/opentk) + [BASS](https://www.un4seen.com/bass.html)

OpenTK provides .NET bindings for [GLFW](https://github.com/glfw/glfw), which handles window creation and input, and OpenGL which allows me to directly use an OpenGL context from .NET

BASS provides the audio decoding and playback + tools for FFT analysis to generate waveforms

This engine was originally created to power [my rhythm game client, Interlude](https://github.com/YAVSRG/Interlude), it has been extracted to be its own library so that
- It is available under MIT license for anyone to use, dissect or otherwise find useful
- It can also power a planned separate rhythm game chart editor
- I can use it for game jams or generally as reference code for other future game projects

I could have used an existing game engine, but making a bespoke engine for my needs provides me with:
- Precise frame timing (on Windows) to ensure very exact smooth motion on a wide range of hardware, beyond what a typical engine may provide
- Precise timings of inputs to audio, beyond what a typical engine may provide
- Fun, as this is my hobby
- Challenge to expand my skills and knowledge

## Features
- Keyboard and mouse input handling, with precise timestamp of keyboard events in relation to backing music
- Precise audio seeking/playback
- Text rendering to an atlas using ImageSharp
- Render engine is optimised around having small number of textures loaded, with most UI being textureless quads and rendered text
- UI framework, basic components, tools for composing UI components to build a game interface
- Game window configuration (fullscreen, window size, resolution, etc)

While I try my best to organise this project to be fairly self-documenting to an experienced game developer, there is very little documentation, even via comments.

If you want documentation or explanations of something for any reason (for example contributing to Interlude), feel free to ask via GitHub issues or contacting me on Discord at @percyqaz
