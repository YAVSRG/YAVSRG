#### Brief overview of parts of the codebase

For people reading it/trying to find it

Projects not mentioned are probably deliberately mentioned because there is nothing of use to you there

If this is not up to date or you can't find what you're looking for, ask in [the Discord](https://yavsrg.net/discord)

## YAVSRG.CLI
a CLI app with a bunch of build tools  
The CI pipelines build it and then run commands like `yavsrg pack_win_x64` to create finished zipped builds of the game

Has some tools to make it easier non-windows users to build and play the game without fiddling about

Also has some tools for me for managing routine tasks like ensuring localisation coverage, adding skins to the repo, etc

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

## Prelude.Tests
Unit tests for prelude, there aren't many atm

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
Both the socket protocol for sending packets live (for multiplayer) and the request formats for the HTTP API

## Interlude.Web
Contains the web server for Interlude online features