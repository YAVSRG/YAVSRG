<h2 align="center">🧱 Building Interlude <code style="color: red; font-size: 20px">&lt;for developers only&gt;</code></h2>

> [!Note]
>
> If the setup guide for non-developers on macOS didn't work, please **do not try these steps** and instead get assistance in [the discord](https://yavsrg.net/discord) if all you want to do is play.

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

4. Use the `debug_run` CLI command to build and test the current branch
   You should run this at least once before building via an IDE.

   After the first use of `debug_run`, you should have the correct BASS .dll/.dynlib/.so files for your platform placed in YAVSRG/interlude/src/bin/Debug/net9.0  
   If not, the game will let you know on launch - Look for them here https://github.com/YAVSRG/YAVSRG/tree/main/engine/lib (you need both the main and the fx one)
   If your platform isn't there contact me in the discord or search online for it

5. **(Optional, highly recommended)**
   If you run the setup script, the CLI tools will be set up as a dotnet tool aliased `yavsrg`
   ```bash
   cd ../scripts # Assuming you're following all steps directly in order
   chmod +x ./setup_cli.sh
   ./setup_cli.sh
   ```
   **Pay attention to the messages from the script**, they may give more instructions on what you need to add to PATH for this work  
   After restarting your terminal, try running `yavsrg debug_run` -- You can run all commands this way or run `yavsrg` to get the same interface as before   

   If you can't get this working, **you can skip this step** and get help in the Discord later, running the tool manually works fine it's just a bit clunkier
   
6. If all that worked, congratulations!
   You can continue to use the `debug_run` command to build and test any changes you make
   OR if you open YAVSRG.sln in [Visual Studio 2022](https://visualstudio.microsoft.com/vs/community/), setting Interlude as the active project will let you simply build and run by pressing F5
   OR the equivalent "build and run" option in your IDE of choice should now generally just work

7. **(Optional)** While Interlude stores all its data in the same folder as the exe by default, you can change that to a specific location - In YAVSRG/interlude/src/bin/Debug/net9.0/config.json you can edit `WorkingDirectory` to whatever is convenient

    I use `"C:/Interlude/dev"` on Windows  
    If you previously built Interlude as a non-developer, setting it to `"../../../../../GAME"` means both builds share data  
    If you previously installed Interlude somewhere else, setting it to that path means both copies share data