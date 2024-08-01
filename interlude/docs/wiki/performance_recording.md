---
title: Recording
folder: Performance
---
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