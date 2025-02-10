---
title: Introduction
folder: Performance
---
# Performance & Framerate

VSRGs are a genre of game where **performance truly matters**.  

These pages aim to explain what settings Interlude provides for performance and how they work, so you can decide what works best on your hardware.

Here are some terms the performance articles use and what they mean:

- *Frames* refer to each single image that gets sent to your screen - Tens or hundreds of frames are produced per second to make a moving image
- *Frame drops* are when the game misses a frame entirely - the notes stay still for a few frames and then jump further along the screen
- *Frame rate* is the number of frames produced by the game per second
- *Frame pacing* refers to how regular the interval between each frame being produced is (with consistency being good)
- *Visual latency* refers to how old the information in a frame is by the time it gets onto your screen

You can see some information about performance and frame times by pressing `F3` ingame.  
`F4` hides/shows the frame time graph, since the graph lowers your frame rate :)  

## Diagnose your problem

*Frame drops* are when your game keeps missing frames.  
This will appear as short freezes and then the notes jumping to catch up with where they should be.  
'Smart' frame cap automatically tries to reduce these as much as possible.  
You can reduce these further by minimising background windows and not running anything else intense in the background on your computer (like another game).

*Frame rate problems* are when your frame rate is too low, resulting in non-smooth motion.  
This is normally when the game cannot produce frames fast enough for the monitor.  
Most users should never experience this as most computers running Interlude can easily produce at least 10x as many frames per second as needed for the screen.

*Frame pacing problems* are when although enough frames are being shown per second, the information shown in each frame is not the same time interval apart.  
This results in notes jittering/not moving smoothly as they might move slightly further in one frame, then slightly less far in the next.  
If you are having this issue, **you could try turning on the Anti-Jitter setting in Options > System > Performance & Framerate**.

*Visual latency problems* are when the frames displayed on your monitor are too out-of-date.  
If this is the case, after pressing a key you would see a significant lag before the screen actually updates in response to your input.  
To reduce the impact on your gameplay, you can compensate for visual latency by **adjusting Visual Offset under Options > System**.  
If it is still too much, see the guide on using 'Unlimited' frame cap.

**Please note:** Visual latency doesn't mean that your inputs are going into the game with a delay, just that the effects show up on the screen with a delay.  
If you can prove you are actually experiencing significant **input lag**, where the game processes your inputs with a delay, please report this in the Discord. 