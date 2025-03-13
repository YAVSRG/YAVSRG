<img src="https://github.com/YAVSRG/YAVSRG/assets/21290233/f3a88ac6-431a-46a1-9351-95266f30fe70.png" align="left" height="240">

### What is YAVSRG?

YET ANOTHER VERTICALLY SCROLLING RHYTHM GAME is my collection of rhythm game projects full of the features and ideas I think make games of this genre fun to play.

The project is centred around my custom rhythm game client, **Interlude**, and the tooling around it.

Visit [yavsrg.net](https://www.yavsrg.net) to read more about this project and its features.

<br/>

<h2 align="center">🎮 Playing the game</h2>

Various player guides, including a wiki, are built into the game to help you get started, especially if you're coming from another popular rhythm game client.  
You can also [check out the wiki here](https://www.yavsrg.net/interlude/wiki) in your browser!

**On Windows and Linux** - Download the latest release [from the website](https://www.yavsrg.net), extract it and play!  
**On macOS** - Not officially supported, to try possibly unstable builds see instructions below

<h2 align="center">🧱 Building Interlude</h2>

> [!Note]
>
> If you just want to play the game on Windows or Linux, **you do *not* need to do this**, instead get the game by downloading the latest release from the site

1. Install [Git](https://git-scm.com/downloads), and [the .NET 9 SDK](https://dotnet.microsoft.com/en-us/download/dotnet/9.0)  
Follow the install instructions for both of these for your platform

2. Set up the repository

	To do this via a terminal:
	```bash
	# Navigate to somewhere you want to store the interlude codebase - Don't forget --recurse-submodules!
	git clone https://github.com/YAVSRG/YAVSRG.git --recurse-submodules
	```
	
3. Run the CLI tool
	```bash
	cd YAVSRG/tools
	dotnet run
	```
	You will see in your terminal something like
	```
	== YAVSRG CLI Tools ==
	type 'help' for a list of commands, or help <command> for details
	>
	```

4. When *in the CLI tool*, run the `play` command to build and play the latest version.  
    This will create a build of the game under `YAVSRG/GAME` which can be run directly if you prefer.  
    To update your game when a new version comes out, run the CLI tool again, then run `update`.
	
**If a step isn't working/you are stuck, get assistance in [the discord](https://yavsrg.net/discord)**

You can delete the entire YAVSRG folder to entirely remove the game and all data from your system.

<h2 align="center">🧱 Building Interlude <code style="color: red; font-size: 20px">&lt;for developers only&gt;</code></h2>

> [!Note]
>
> If the setup guide for non-developers on Linux/macOS didn't work, please **do not try these steps** and instead get assistance in [the discord](https://yavsrg.net/discord) if all you want to do is play.

Developers should first follow the non-developer setup and ensure the CLI tool works.  
Use the `debug_run` CLI command to build and test the current branch - You should run this at least once before building via an IDE.

After the first use of `debug_run`, you should have the correct BASS .dll/.dynlib/.so files for your platform placed in YAVSRG/interlude/src/bin/Debug/net9.0  
If not, the game will let you know on launch - Look for them here https://github.com/YAVSRG/YAVSRG/tree/main/engine/lib (you need both the main and the fx one)
If your platform isn't there contact me in the discord or search online for it

If you open YAVSRG.sln in [Visual Studio 2022](https://visualstudio.microsoft.com/vs/community/), setting Interlude as the active project will let you simply build and run by pressing F5.

While Interlude stores all its data in the same folder as the exe by default, you can change that to a specific location - In YAVSRG/interlude/src/bin/Debug/net9.0/config.json you can edit `WorkingDirectory` to whatever is convenient

I use `"C:/Interlude/dev"` on Windows  
If you previously built Interlude as a non-developer, setting it to `"../../../../../GAME"` means both builds share data  
If you previously installed Interlude somewhere else, setting it to that path means both copies share data

<h2 align="center">🤝 Contributing</h2>

Come join [the discord](https://yavsrg.net/discord) -- Send me playtesting feedback, feature requests, bug reports, etc.

Once up and running with the developer build instructions above, steps for contributing are outlined [here](https://github.com/YAVSRG/YAVSRG/tree/main/docs/contributors.md)

Pull requests are very welcome!
If you spot a bug, typo or other minor problem and have a quick fix, feel free to submit a PR.
If you want to work on a feature please check with me first via [Discord](https://yavsrg.net/discord)

> [!Warning]
>
> IMPOSTOR SYNDROME DISCLAIMER
>
> Don't know F#? Don't know how to code but want to learn? This is a hobby project largely dedicated to expanding my skills as a programmer and I would be pleased if it could do the same for you.
>
> Ask away in the Discord, I'm often free to chat about how I or the codebase work and will happily give YOU the tools to make meaningful contributions.
