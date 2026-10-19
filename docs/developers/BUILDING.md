## Requirements

- [Git](https://git-scm.com/downloads) **including Git Bash if you're on Windows (option in the installer)**
- [.NET 9 SDK](https://dotnet.microsoft.com/en-us/download/dotnet/9.0) **be sure you get the SDK, not the Runtime**

Follow the install instructions for both of these for your platform

**Instructions written in code blocks are commands to be executed in a terminal:**
```bash
echo 'example'
```

----

## Building

### 1. Clone the repository

First navigate somewhere you want to store the codebase, then:
```bash
git clone https://github.com/YAVSRG/YAVSRG.git --recurse-submodules
cd YAVSRG
```

[FAQ: Forgot --recurse-submodules](#i-forgot---recurse-submodules-when-checking-out-now-what)
	
### 2. Install required dotnet tools

```bash
dotnet tool restore
```
Successful result: `Restore was successful.`
	
### 3. Build and run the game once

```bash
./scripts/yavsrg.sh run
```

[FAQ: Permission denied](#permission-denied-error)

[FAQ: Missing binaries for your platform](#missing-dlls)

If the game opens, you're all set! See the [contributor guide](CONTRIBUTING.md) for the process of submitting fixes or features

After this, usual `dotnet run` and `dotnet build` commands will work correctly, and your IDE of choice should be able to build and run the game  
If you don't have a preferred IDE, I would recommend [JetBrains Rider](https://www.jetbrains.com/rider/) for its good F# support
   
### 4. **(Optional)** Add `yavsrg` command to your path

If you are working in multiple worktrees at once, stick to running the script directly as that will run in the worktree it's being executed in  
This adds `yavsrg` globally so you can run commands quickly e.g. `yavsrg run` vs `./scripts/yavsrg.sh run`:
```bash
./scripts/yavsrg.sh update_cli
```
Successful result: `You can invoke the tool using the following command: yavsrg`

Test it installed correctly:
```bash
yavsrg help
```
You should see a list of commands, rather than a not found error

[FAQ: Command not found](#dotnet-tool-yavsrg-not-found)
   
### 5. **(Optional)** Share data with another installation of Interlude

In YAVSRG/interlude/src/bin/Debug/net9.0/config.json you can edit `WorkingDirectory` to whatever is convenient

I use `"C:/Interlude/dev"` on Windows  

If you previously installed Interlude somewhere else, setting it to that path means both copies share data

## Common issues

### I forgot --recurse-submodules when checking out, now what?

This will fix them:
```bash
git submodule update --init libraries/Percyqaz.Common libraries/Percyqaz.Data libraries/Percyqaz.Shell
```

### Permission denied error

If running `yavsrg.sh` gives a permission error/cannot be executed, give it execute permission
```bash
chmod +x ./scripts/yavsrg.sh
```

### Missing DLLs

The `run` command ensures you have the correct libraries for your platform placed in YAVSRG/interlude/src/bin/Debug/net9.0  
If not, the game will let you know on launch - Look for them here https://github.com/YAVSRG/YAVSRG/tree/main/engine/lib (you need both the main and the fx one)

If your platform isn't there contact me in the discord or search online for it

### Dotnet tool yavsrg not found

Try this first, it will temporarily fix it until you reopen the terminal:
```bash
export PATH=$HOME/.dotnet/tools:$PATH
```

If that fixed it, the path dotnet tools install in isn't added to your PATH environment variable  
See https://learn.microsoft.com/en-us/dotnet/core/tools/dotnet-tool-install#global-tools

To permanently fix it, add the line `export PATH=$HOME/.dotnet/tools:$PATH` to your terminal config  
This will depend on your terminal setup and operating system