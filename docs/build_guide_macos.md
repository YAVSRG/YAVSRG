<h2 align="center">🧱 Building Interlude</h2>

This is the build guide targeted at non-developers on macOS who want to try unstable builds of the game

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
    You should see:
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