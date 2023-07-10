## YAVSRG

My latest attempt at managing a project with several parts that need their own release streams but are developed together in one place on my machine (I am sick of both monorepos and submodules for many reasons)

#### How to set up
```bash
git clone https://github.com/YAVSRG/YAVSRG.git
./clone-repos.sh
# Cloning Interlude.Assets will fail because it's private, that's OK you won't need it
```
After setting up, you can open `YAVSRG.sln` in your IDE of choice to open the whole multi-repo project at once.

Using an IDE that lets you commit to multiple repositories at once is recommended

#### Build Interlude (developers only)
**YOU DON'T NEED TO DO THIS IF YOU AREN'T A DEVELOPER**

To play the game, download the latest release

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