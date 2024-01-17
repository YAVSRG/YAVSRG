- Use SQLite to store various data (mostly scores) IN PROGRESS
- Discord rich presence! Library I found for this has an issue where it tries to reconnect every 10 seconds if discord is not running, lag spiking the game
- Requested: Non-pitch rates. Not a priority :)
- Maybe hitsounds or clap tracks at some point
- Go back and revisit all Percyqaz.Flux startup logic, would be nice interlude explicitly loads everything it can before window is created, then when the window is loaded it has a GL context so it can load textures at that point -- Generally break stuff into `windowless_init` and `window_init` and be careful and attentive with it

### [[The Paths]]

more [[Prelude]] Features
Things that would be cool to have a library for even if I don't immediately use it in Interlude - Could be used in other rhythm game stuff I do

### [[Pattern generation]]
