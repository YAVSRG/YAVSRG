---
title: Introduction
folder: Skinning
---
# Skins

A **skin** is a bundle of files that customise how gameplay looks.  

A skin contains one or both of: A **HUD** and a **Noteskin**.  

All skin textures are actually a "grid" of individual sprites - this allows for 'variants' of a texture and animation frames for those variants.  

A grid can be a single `.png` file, with a suffix indicating how many sprites are in it, for example:

- `note[1x1].png` stores all the `note` sprites, and there is only 1 sprite.  
- `note[8x16].png` stores all the `note` sprites, there are 8 variants (for notes this means different colors), and 16 animation frames per color.

This single-file approach is similar to how Stepmania/Etterna noteskins store textures.

If you prefer, each sprite can simply be its own file instead:

- `note-0-0.png` represents the first frame of the first note color
- `note-3-1.png` represents the second frame of the fourth note color
- `note-0-5.png` represents the sixth frame of the first note color

This multiple-file approach is similar to how osu! skins store textures.

Each texture can independently be in single-file form or multiple-file form, depending on what suits you best.  
*You can switch a texture between these modes by using the ingame skin editor tools.*

You can directly edit `.png` files with any image editor (If you don't have one, some free options are [GIMP](https://www.gimp.org/) or [Paint.NET](https://www.getpaint.net/)).

A skin is a folder with:

- A `skin.json` file containing metadata about the skin (its name and author)
- An optional `HUD` folder containing HUD config and textures
- An optional `Noteskin` folder containing Noteskin config and textures

Click here to read more about [Noteskins](skinning_noteskins.html).  
Click here to read more about [HUD layouts](skinning_huds.html).  