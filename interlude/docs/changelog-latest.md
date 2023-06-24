0.6.20
====

More things you asked for

# 'Vanishing notes' mode
LNs should also pass the receptors if not hit, and snap into the receptors when hit. They don't at the moment, this is a BUG
Unfortunately my render code is very old and delicate and I will need a big brain to fix this

I did however fix a bug with previews - All notes now look like they are being hit perfectly on previews

# Other bug fixes, improvements
- From now on, any newly cached charts will not break if the Interlude folder is moved somewhere else
  Run Debug > Rebuild cache in the options menu to apply this fix to your existing charts
- Optimised the level select screen. For very big open folders (10,000+) charts you should see double the framerate
- Optimised the 'What's new' screen. As changelogs piled up the screen was starting to run at about 30fps

# New and improved: Progress meter
Turns out peppy had it right all along, the best way to display chart progress is the familiar pie chart
The progress meter is now a pie chart, you can reposition it, change the size and colors, etc like the rest of the HUD

!! Existing users: You should click the 'Reset' button on the positioning for your Progress Meter as it will be using the old position of the falling dot

