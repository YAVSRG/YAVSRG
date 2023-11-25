# Yet Another Vertically Scrolling Rhythm Game

The YAVSRG project encompasses my custom rhythm game client, [Interlude](https://github.com/YAVSRG/Interlude), and the tooling around it

See [yavsrg.net](https://www.yavsrg.net) for more information on this project.

This repo is my latest go at managing a solution of several parts that I can easily work on and test together on my machine, but could otherwise be checked out as separate repos

The struggle of submodules:
- **Interlude.Web** and **Interlude** projects both depend on **Prelude** as a submodule. This would mean I need to have two copies of the repo checked out (unless I do something wacky with symlinks?) and constantly keep them synchronised as I want to test changes to all 3 projects together.  
  This is one instance of a few dependency tree scenarios solved by just cloning all repos side-by-side

The struggle of monorepo:
- Size of cloning everything for CI pipelines
- Cannot release versions of both game client and chart editor via GitHub releases in future if they share a repo
- Reusable libraries that belong on my personal GitHub outside the YAVSRG org would Have to stay as submodules
- Some private assets/scripts still need to be submodules
- Submodules make me go mental and I want them gone from or abstracted out of my workflow

----

### ⚙️ Set up guide ***(For developers)***
```bash
git clone https://github.com/YAVSRG/YAVSRG.git
./clone-repos.sh
# Cloning Interlude.Assets will fail because it's private, that's OK you won't need it
```
After setting up, you can open `YAVSRG.sln` in your IDE of choice to open the whole multi-repo project at once.

Using an IDE that lets you commit to multiple repositories at once is recommended

### 🤖 Building Interlude ***(For developers)***
> [!Note]
>
> If you just want to play the game, **you do *not* need to do this**, instead get the game by downloading the latest release from the site

Build for development:
```
cd Interlude
./interlude.sh
# The game will crash upon first startup, because
# you need the correct bass.dll/dynlib/so for your platform placed in ./src/bin/Debug/net7.0
# Get it here https://github.com/percyqaz/Percyqaz.Flux/tree/master/lib
# If your platform isn't there contact me in the discord or search online for it
```

Run the same toolchain that CI does to make a distributable build:
```bash
cd Interlude/tools
dotnet run -- release_win64
cd ../releases
# interlude_win64.zip is now in here
```
