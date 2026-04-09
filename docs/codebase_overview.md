#### Brief overview of parts of the codebase

For people reading it/trying to find it

Projects not mentioned are probably deliberately mentioned because there is nothing of use to you there

If this is not up to date or you can't find what you're looking for, ask in [the Discord](https://yavsrg.net/discord)

## YAVSRG.CLI
Contains meta-tools to help with working on other projects in this repository
Can be installed as a `yavsrg` command on the command line, and contains tools to:
- Create finished zipped builds of the game
- Check translation files are up to date/perform renaming operations
- Perform first-time setup for developers who want to build and run the client/server locally
- Various tools just for me for maintenance purposes

## Prelude
Core library for Interlude, it's a library which means its functions can be used independently of the game client  
i.e. someone could make a tool using Prelude that uses its ability to parse osu! storyboards, or to manipulate Interlude noteskins  

It contains code for:
- Chart formats and conversions
- Chart mods
- Score calculations
- Score database storage
- Chart caching, sorting, grouping, searching, filtering
- Difficulty calculation and pattern analysis

## Prelude.Playground
Console app where I write scripts to hand-test Prelude features
Also contains some historical projects where I've used Prelude as a tool for generating storyboards/custom osu! beatmaps which can serve as examples

# Percyqaz.Flux
Core game engine and framework
It is a separate library so I can reuse it as a component for future projects (like a chart editor, and some future other games maybe)

It contains code for:
- Creating a game window
- Handling all input, rendering, audio
- Stuff for building a UI out of components called Widgets

## Interlude
The game client

Consists of:
- A UI built on Percyqaz.Flux's system
- Loading game content via Prelude
- Features powered by Prelude

## Interlude.Web.Shared
Contains the online protocol for Interlude client <-> Interlude server
Both the socket protocol for sending packets live (for multiplayer) and the request formats for the Web API

## Interlude.Web
Contains the web server for Interlude online features, which hosts:
- A discord bot primarily for admin commands and monitoring
- Multiplayer lobby server
- Web API and database for score submission