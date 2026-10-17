0.7.28.5
====

:o

# Features
- Added a logarithmic scale option to Error bar/Column error bars by @Muffletruffle
- Uploaded scores can now be up to 3.0x rate instead of the previous 2.0x max by @Muffletruffle
- Grade A color on wife rulesets now more accurate by @Muffletruffle
- Chart pattern detail bars now scale to song duration by @Muffletruffle
- Exact hold count and percentage are now both shown on level select by @Muffletruffle
- Audio crossfade between songs can now be disabled under Audio settings

# Bug fixes
- Fixed rate HUD element not updating when rate changed in practice mode
- Fixed many case-sensitive file issues when importing osu! skins on Linux
- Fixed some .osk files not importing because they contain nested folders with the skin inside
- Fixed lots of small parity problems when importing/exporting .osu files
- Fixed a bigger parity problem with .osu files where SV lines had a max multiplier of 100 instead of 10
- Fixed the 1 frame "jump" notes would sometimes make, always in the first two seconds of playing a song
- Fixed all newly loaded songs playing out of the default audio device; changing the setting only applying to the current song
- Fixed issues with mounted imports registering as "never successfully imported" if interrupted during a reimport
- Fixed the early/late HUD element fading animation by @Muffletruffle
- Fixed crash when navigating between sessions on the stats screen by @Muffletruffle

