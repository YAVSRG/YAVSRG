---
title: Performance
folder: Getting started
---
# Performance & Framerate

VSRGs are a genre of game where **performance truly matters**.  

This page aims to explain what settings Interlude provides for performance and how they work, so you can decide what works best on your hardware.

Here are some terms this article uses and what they mean:
- *Frames* refer to each single image that gets sent to your screen - Tens or hundreds of frames are produced per second to make a moving image
- *Frame drops* are when the game misses a frame entirely - the notes stay still for a few frames and then jump further along the screen
- *Frame rate* is the number of frames produced by the game per second
- *Frame pacing* refers to how regular the interval between each frame being produced is (with consistency being good)
- *Visual latency* refers to how old the information in a frame is by the time it gets onto your screen

You can see some information about performance and frame times by pressing `CTRL+SHIFT+ALT+F3` ingame.  
`CTRL+SHIFT+ALT+F4` hides/shows the frame time graph, since the graph lowers your frame rate :)  

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
If it is still too much, see the guide further down on using 'Unlimited' frame cap.

**Please note:** Visual latency doesn't mean that your inputs are going into the game with a delay, just that the effects show up on the screen with a delay.  
If you are actually experiencing significant **input lag**, where the game processes your inputs with a delay, please report this in the Discord. 

::::

## Using 'Unlimited' frame cap

If you are having problems with 'Smart' frame cap (such as particularly high visual latency), you can *entirely uncap the engine to produce as many frames per second as possible*.  
This should be used at your own risk as it can potentially make your graphics card hot as if running a much bigger game or system benchmark.

If you have an NVIDIA graphics card you can add a frame cap in your graphics drivers to prevent the GPU getting as hot/using as much power, which I **strongly recommend**.  
Adding a frame cap directly in the drivers is far more effective than any frame cap I could code into the game engine.

Here's how:

- Open the start menu, find and open NVIDIA Control Panel
- On the left hand sidebar, select 'Manage 3D settings'
- On the 'Manage 3D Settings' page, switch to the 'Program Settings' tab
- Select Interlude.exe in the dropdown of what program to customise
- Find 'Max Frame Rate' and change it to On, 1000FPS (experiment with other values if you like)
- Click 'Apply' in the bottom right
- Now **restart the game** and switch to 'Unlimited' frame cap after restarting

If you don't have an NVIDIA graphics card you will have to figure out equivalent steps by Googling or asking in the Discord.

::::

## Keyboard input
 
Interlude checks for keyboard inputs as fast as possible in a background thread.  
Because of this, keyboard input in Interlude is **almost entirely unrelated** to your frame rate, and low frame rate does not mean the game polls your inputs slower.

Your keyboard should poll at 1000hz - If your keyboard goes higher that's a nice bonus but won't make a real difference.  
Your keyboard should NOT poll at 125hz - That is too slow and will lead to your taps being measured inaccurately!

[This video](https://www.youtube.com/watch?v=heZVmr9fyng) has some good info on the topic, including how to tell if you're on 125hz.

All in all as long as your keyboard polls at 1000hz (most gaming keyboards) you're good to go.

Often players will report render settings as causing "input lag" or "input latency", but this is not to do with your keyboard or how the game registers inputs.  
Normally, what is actually happening is **visual latency** from displaying frames that were rendered a few milliseconds ago - Your monitor is not yet showing the effect your keyboard input had!

::::

## Streaming or recording the game

You're probably using OBS.

OBS captures frames directly from the game so it doesn't see the exact same thing the monitor sees.  
Because of this and how the game tries to sync with your monitor, you may see stuttering or glitching in your recorded video that you don't see on your monitor.

I'm still looking into this and don't want to offer any misinformation, but what works for me is:

- Smart frame limit
- Borderless windowed mode
- In OBS, use a 'Display Capture' instead of a 'Game Capture'
- In OBS, right-click on the stream preview and *disable it*
- Minimise the OBS window before recording, if convenient

With those settings I was able to record/stream perfectly smooth 60fps gameplay on my 60hz monitor without visual artifacts.