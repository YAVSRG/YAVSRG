---
title: Advanced settings
folder: Performance
---
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