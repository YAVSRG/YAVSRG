---
title: Performance
folder: Getting started
---
# Performance

VSRGs are a genre of game where **performance truly matters**.  
Frame drops and stutters can be disorienting, so can a delay between when you press a key and its impact on your monitor.

This page aims to explain what settings Interlude provides for performance and how they work, so you can decide what works best on your hardware.

### Short summary for the busy reader
For the average user, I recommend using the 'Smart' frame limit.  
Fullscreen is recommended, but input latency is constant and can be compensated for with Visual Offset in windowed modes.  
Higher refresh rate monitors are better and have lower latency.  

You can see some information about performance and frame times by pressing `CTRL+SHIFT+ALT+F3` ingame.  
`CTRL+SHIFT+ALT+F4` hides/shows the frame time graph, since the graph lowers your framerate :)  

Now check out below if you want to know more about how the engine works!

::::

# Introduction

Outline of some background knowledge and key terms used further down in this guide.

## Fullscreen vs Windowed

The game renders "frames" - These are single images that your monitor cycles through to show the game in motion.  
"**frames per second**" refers to how many frames, on average, the game outputs per second. 

Your monitor has a refresh rate - This is how many "frames" the monitor can switch through per second, for example a 60hz monitor can display a new frame every 60th of a second, or 16.666 milliseconds.  
The monitor "scans" these frames in line by line, from top to bottom (this can only be seen with a slow motion camera), and will happily switch frame mid-scan if it receives a new one.

In (borderless) windowed mode, Window's window manager will wait until the monitor starts its next "scan", and then send the latest frame the game rendered to the monitor.
This waiting technique, known as VSync, means a frame but only gets shown when the monitor is next ready for another frame.
This introduces **visual latency** between what the game has rendered and what shows on your monitor!  
In exchange for this delay, the monitor never changes frame midway through scanning in the last frame it was sent, so you cannot see **screen tearing** while in windowed modes.

In fullscreen, the game takes full control of your monitor and can directly send rendered frames to it.
This means the monitor can receive a new frame while it is halfway through switching to the previous one.  
This causes **screen tearing** where one part of the screen is an older frame, while the rest of the screen lower down is a newer frame.

Either way you open yourself up to possible issues that may hamper your smooth and enjoyable game experience.  
To mitigate these issues, Interlude has a 'Smart' frame limiter mode, but you can also uncap your framerate if you know hardware/driver settings that work better for you.

## Keyboard input
 
Interlude checks for keyboard inputs as fast as possible in a background thread.  
Because of this, keyboard input in Interlude is **almost entirely unrelated** to your frame rate, and low frame rate does not mean the game polls your inputs slower.

Your keyboard should poll at 1000hz - If your keyboard goes higher that's a nice bonus but won't make a real difference.  
Your keyboard should NOT poll at 125hz - That is too slow and will lead to your taps being measured inaccurately!

[This video](https://www.youtube.com/watch?v=heZVmr9fyng) has some good info on the topic, including how to tell if you're on 125hz.

All in all as long as your keyboard polls at 1000hz (most gaming keyboards) you're good to go.

Often players will report render settings as causing "input lag" or "input latency", but this is not to do with your keyboard or how the game registers inputs.  
Normally, what is actually happening is **visual latency** from displaying frames that were rendered a few milliseconds ago - Your monitor is not yet showing the effect your keyboard input has had!

::::

## 'Smart' frame limit

This mode caps Interlude's framerate to your monitor's refresh rate.  
A frame cap on its own is not suitable, because small timing errors mean frames will not perfectly line up with your monitor's refresh cycle.

In windowed mode, if frames don't line up with your monitor's refresh rate:

- The delay between when a frame gets rendered and when it ends up on your monitor will drift over time, making it feel like your visual offset is changing over time
- If the frame cap is slightly too slow, a frame will periodically be displayed for twice as long
- If the frame cap is slightly too fast, a frame will periodically be skipped because another frame arrives before it is used

In fullscreen mode, frames need to arrive right about when the monitor renders a new frame or the you will get **screen tearing**.

Smart frame limit aims to compensate for these issues by:

- In fullscreen, making timing adjustments every frame to ensure the "tear line" is very close to the top/bottom of your monitor where you can't see it.
  This results in low latency, smooth rendering with occasional visual artifact if the "tear line" moves!
- In windowed mode, making timing adjustments every frame so that the visual delay is a consistent value
- In both modes, making **anti-jitter** render adjustments every frame which makes notes look smoother

I'm quite pleased with the achievements of the 'Smart' frame limit, but if you're a game developer or have deeper knowledge about this kind of thing then suggestions are always very much welcome.

::::

## Unlimited frame limit

This mode uncaps Interlude's framerate.  
The game will render as many frames as it can, as fast as possible.

In windowed mode, when the monitor is ready to refresh, it will draw the most recent frame it received from the game.  
For example, if the game is running at 1000fps, the most recent frame will be generated in the last 1ms, and so will be at most 1ms old when it appears on your monitor.  
**This is currently the best way I know to reduce input latency**, but I still recommend 'Smart' frame limit because:

- Frames can take varying time to render, which also means the information in a frame can be slightly older or newer by 1-2ms
  This causes **jitter** where notes do not look like they are scrolling perfectly smoothly, even though all other metrics say you are getting the right framerate!
- The age of the completed frame you see will vary between 0-1ms old when displayed on the monitor, also causing frame **jitter**
- Unlimited frame cap can be very power intense!
- You will get constant, random screen tearing in fullscreen mode
- The visual latency caused by 'Smart' frame limit is constant, predictable, and can be compensated for with the Visual Offset setting.

For some hardware/system optimisation nerds you may be able to get good use out of Unlimited frame cap if you have better ways around these issues than 'Smart' cap does.  
Some experimentation needed if you want to try this.

### Reducing jitter

The main source of jitter is frames taking different times to render.  
This can be reduced by running fewer other programs and background tasks while Interlude is open.

If you've ever tried to optimise your system for other competitive games such as Counter-Strike, you probably know how to do this.  
If not, those kind of guides are a good place to find information.

### Reducing power usage

I highly recommend applying a frame cap to Interlude at the graphics driver-level, especially if you have a powerful machine that can render way above 1000 frames per second.  
Most of you will be using an NVIDIA graphics card - In NVIDIA control panel you can directly set a frame cap on Interlude.exe under Manage 3D settings > Program settings.

### Reducing screen tearing (in Fullscreen)

I don't know a way to do this - The tearing you get is in exchange for low visual latency.  
Your monitor or graphics drivers may have some special settings or modes that can help with this!

Remember that VSync will introduce some visual latency, in which case you may get better results by using 'Smart' frame limit instead.

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
- I also ran OBS with high system priority, in admin mode, but try without this step first

With those settings I was able to record/stream perfectly smooth 60fps gameplay on my 60hz monitor without visual artifacts.