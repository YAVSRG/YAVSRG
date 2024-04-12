<img src="https://github.com/YAVSRG/YAVSRG/assets/21290233/f3a88ac6-431a-46a1-9351-95266f30fe70.png" align="left" height="240">

## What is YAVSRG?
YET ANOTHER VERTICALLY SCROLLING RHYTHM GAME is a collection of rhythm game projects full of the features and ideas I think make games of this genre fun to play.

The project is centred around my custom rhythm game client, **Interlude**, and the tooling around it.

Visit [yavsrg.net](https://www.yavsrg.net) to read more about this project and its features.

<br/>

<h2 align="center">🎮 Playing the game</h2>

Various player guides, including a wiki, are built into the game to help you get started, especially if you're coming from another popular rhythm game client.  
You can also [check out the wiki here](https://www.yavsrg.net/interlude/wiki) in your browser!

**On Windows** - Download the latest release [from the website](https://www.yavsrg.net), extract it and play!  
**On macOS** - Build instructions for developers below! If you aren't a developer, ask for help [in the Discord](https://discord.gg/tA22tWR) instead  
**On Linux** - Build instructions for developers below! If you aren't a developer, ask for help [in the Discord](https://discord.gg/tA22tWR) instead  

On non-Windows platforms you could also try running the Windows build in WINE or Proton.

<h2 align="center">🧱 Building Interlude <code style="color: red; font-size: 20px">&lt;for developers only&gt;</code></h2>

> [!Note]
>
> If you just want to play the game on Windows, **you do *not* need to do this**, instead get the game by downloading the latest release from the site

Cloning the codebase to your machine requires [Git](https://git-scm.com/downloads), and building requires [the .NET 8 SDK](https://dotnet.microsoft.com/en-us/download/dotnet/8.0)  
Follow the install instructions for your platform

To check out the codebase on your machine:
```bash
# navigate to somewhere you like to store code
git clone https://github.com/YAVSRG/YAVSRG.git --recurse-submodules
```
If you forgot to use `--recurse-submodules` when you cloned, you can manually initialise the submodules:
```
git submodule update --init libraries/Percyqaz.Common
git submodule update --init libraries/Percyqaz.Data
git submodule update --init libraries/Percyqaz.Shell
```

To build and run the game, run the build script:
```bash
cd scripts
chmod +x interlude_run.sh
./interlude_run.sh
# On first startup, the game will tell you that you need the correct bass.dll/dynlib/so for your platform placed in ./interlude/src/bin/Debug/net8.0
# Look for it here https://github.com/YAVSRG/YAVSRG/tree/main/engine/lib
# If your platform isn't there contact me in the Discord or search online for it
```

To create a distributable zip file of the game, run the toolchain project:
```bash
cd interlude/tools
dotnet run -- release_win64
cd ../releases
# interlude_win64.zip is now in the current directory
# these are the same build artifacts used in the release pipeline
```

<h2 align="center">🤝 Contributing</h2>

Come join [the discord](https://discord.gg/tA22tWR) -- Send me playtesting feedback, feature requests, bug reports, you name it

Pull requests are very welcome!
If you spot a bug, typo or other minor change and have a quick fix, just go right ahead and PR it.
If you want to work on a feature please check with me first via [Discord](https://discord.gg/tA22tWR)

> [!Warning]
>
> IMPOSTOR SYNDROME DISCLAIMER
>
> Don't know F#? Don't know how to code but want to learn? This is a hobby project largely dedicated to expanding my skills as a programmer and I would be pleased if it could do the same for you.
>
> Ask away in the Discord, I'm often free to chat about how I or the codebase work and will happily give YOU the tools to make meaningful contributions.
