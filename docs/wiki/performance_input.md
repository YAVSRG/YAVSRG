---
title: Input
folder: Performance
---
## Keyboard input
 
Interlude checks for keyboard inputs as fast as possible in a background thread.  
Because of this, keyboard input in Interlude is **almost entirely unrelated** to your frame rate, and low frame rate does not mean the game polls your inputs slower.

Your keyboard should poll at 1000hz - If your keyboard goes higher that's a nice bonus but won't make a real difference.  
Your keyboard should NOT poll at 125hz - That is too slow and will lead to your taps being measured inaccurately!

[This video](https://www.youtube.com/watch?v=heZVmr9fyng) has some good info on the topic, including how to tell if you're on 125hz.

All in all as long as your keyboard polls at 1000hz (most gaming keyboards) you're good to go.

Often players will report render settings as causing "input lag" or "input latency", but this is not to do with your keyboard or how the game registers inputs.  
Normally, what is actually happening is **visual latency** from displaying frames that were rendered a few milliseconds ago - Your monitor is not yet showing the effect your keyboard input had!