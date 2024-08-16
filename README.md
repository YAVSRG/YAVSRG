<img src="https://github.com/YAVSRG/YAVSRG/assets/21290233/f3a88ac6-431a-46a1-9351-95266f30fe70.png" align="left" height="240">

### What is YAVSRG?

YET ANOTHER VERTICALLY SCROLLING RHYTHM GAME is my collection of rhythm game projects full of the features and ideas I think make games of this genre fun to play.

The project is centred around my custom rhythm game client, **Interlude**, and the tooling around it.

Visit [yavsrg.net](https://www.yavsrg.net) to read more about this project and its features.

<br/>

<h2 align="center">🎮 Playing the game</h2>

Various player guides, including a wiki, are built into the game to help you get started, especially if you're coming from another popular rhythm game client.  
You can also [check out the wiki here](https://www.yavsrg.net/interlude/wiki) in your browser!

**On Windows** - Download the latest release [from the website](https://www.yavsrg.net), extract it and play!  
**On macOS and Linux** - You currently must build the game from source, I've made this quick and easy, see below

<h2 align="center">🧱 Building Interlude</h2>

> [!Note]
>
> If you just want to play the game on Windows, **you do *not* need to do this**, instead get the game by downloading the latest release from the site

1. Cloning the codebase to your machine requires [Git](https://git-scm.com/downloads), and building requires [the .NET 8 SDK](https://dotnet.microsoft.com/en-us/download/dotnet/8.0)  
Follow the install instructions for both of these for your platform

2. Run the setup scripts

To do this via terminal on macOS or linux:
```bash
# Navigate to somewhere you want to store the interlude codebase
git clone https://github.com/YAVSRG/YAVSRG.git
cd YAVSRG/scripts
chmod +x ./setup_repo.sh
./setup_repo.sh
chmod +x ./setup_cli.sh
./setup_cli.sh
```
On windows terminal, skip the `chmod` lines, or run the scripts through other means like double clicking to run them in Git Bash

The steps above should set up the `yavsrg` CLI command. Try running `yavsrg version`, you should see a version number output in the terminal.  
**If this hasn't worked and you are stuck, get assistance from me in [the discord](https://discord.gg/tA22tWR)**

3. Run `yavsrg play` to build and play the latest version.  
   From now on `yavsrg play` will launch the game when you want to play.  
    To update your game when a new version comes out, run `yavsrg update`.
	
To later uninstall the `yavsrg` command line tool, run `dotnet tool uninstall --global yavsrg.cli`  
After that you can delete the entire YAVSRG folder to entirely remove the game and all data from your system.

<h2 align="center">🧱 Building Interlude <code style="color: red; font-size: 20px">&lt;for developers only&gt;</code></h2>

> [!Note]
>
> If the setup guide for non-developers on Linux/macOS didn't work, please **do not try these steps** and instead get assistance in [the discord](https://discord.gg/tA22tWR) if all you want to do is play.

The `yavsrg update` command will checkout and build the latest release, if you are a developer you will want to build what is on your local branch instead.

Use `yavsrg debug_run` to build and test the current branch, or [go and look inside at what it does](https://github.com/YAVSRG/YAVSRG/blob/main/tools/Client/Play.fs) and use an IDE feature accordingly.

On first startup via `yavsrg debug_run`, the game will tell you that you need the correct bass.dll/dynlib/so files for your platform placed in YAVSRG/interlude/src/bin/Debug/net8.0  
Look for them here https://github.com/YAVSRG/YAVSRG/tree/main/engine/lib (you need both the main and the fx one)
If your platform isn't there contact me in the discord or search online for it

If you open YAVSRG.sln in [Visual Studio 2022](https://visualstudio.microsoft.com/vs/community/), setting Interlude as the active project will let you simply build and run with F5 which is my current workflow.

While Interlude stores all its data in the same folder as the exe by default, you can change that to a specific location - In YAVSRG/interlude/src/bin/Debug/net8.0/config.json you can edit `WorkingDirectory` to whatever is convenient

I use `"C:/Interlude/dev"` on Windows  
If you previously built Interlude as a non-developer, I recommend setting it to `"../../../../../GAME"` so both builds share data

<h2 align="center">🤝 Contributing</h2>

Come join [the discord](https://discord.gg/tA22tWR) -- Send me playtesting feedback, feature requests, bug reports, etc. I'm active a lot in 

Once up and running with the developer build instructions above, steps for contributing are outlined [here](https://github.com/YAVSRG/YAVSRG/tree/main/docs/contributors.md)

Pull requests are very welcome!
If you spot a bug, typo or other minor problem and have a quick fix, feel free to submit a PR.
If you want to work on a feature please check with me first via [Discord](https://discord.gg/tA22tWR)

> [!Warning]
>
> IMPOSTOR SYNDROME DISCLAIMER
>
> Don't know F#? Don't know how to code but want to learn? This is a hobby project largely dedicated to expanding my skills as a programmer and I would be pleased if it could do the same for you.
>
> Ask away in the Discord, I'm often free to chat about how I or the codebase work and will happily give YOU the tools to make meaningful contributions.
