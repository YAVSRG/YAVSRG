---
title: StepMania/Etterna
folder: Migrating
---
# Migrating from StepMania or Etterna

Already play StepMania or Etterna (or both)? This guide will walk you through migrating your songs to Interlude.  

If you encounter any bugs or things that don't look right, feel free to ask in the Discord server.

::::

## Step 1: Songs

In Options > Library, there is a tool for linking your existing Songs library to Interlude.  
It will first try to auto detect the folder for the respective game, but if that doesn't work you can just link by dropping your **Songs folder** onto the Interlude window **when the menu asks you to.**

By linking your libraries instead of fully importing them, audio files and background images are not copied, so the Interlude version of the song library should only be a few MB (saves on drive space if you play both).  
There is also an option to automatically import newly added charts whenever Interlude launches.

It may take a few minutes to complete the import, but it will work in the background and you can play what songs have already been imported.

You can also download new Etterna packs ingame via the Import screen.

::::

## Step 2: Noteskins

With a small bit of knowledge and the [Noteskins guide](noteskins.html), you should have no problem porting the vast majority of StepMania/Etterna noteskins to Interlude by renaming files.  
In the future there may be a tool to automate or partially automate the process.

Alternatively, there are noteskins available for download from the Import screen - If you use a common skin it may already be there!

::::

## Step 3: Scroll speed and audio sync

It's hard to conveniently convert your scroll speed to Interlude, so I recommend eyeballing it.  
Under Options > Gameplay > Scroll speed, there is an indicator of what Interlude scroll speed is equal to what osu! scroll speed, but it **is only accurate if your column width is the same in both games** due to how Etterna's scroll speed calculation works.

On the Level select menu, you can open Practice mode to play a section of the selected song and the game will suggest adjustments to your scroll speed, hitposition, audio offset and visual offset.  
If you're used to another game, you should adjust all offsets with the game audio muted at first, and then change audio offset last.

Suggested order to make changes:

- Mute game audio while playing a section of a chart in practice mode
- Once scroll speed looks right to you, adjusting hitposition until it looks right
- Once hitposition looks right to you, adjust visual offset if needed
- Unmute game audio and continue playing to adjust audio offset as needed