[[Interlude.Web]]

Multiplayer is largely working and stable, allowing for [[Spectating]] in lobbies while you wait and a few other nifty things

Here's my todo list:
- Support selecting [[Mods]]
	- The host picks and everyone has to use the same mods at first
	- Later, everyone can freely pick mods (maybe as an option the host can opt-in for)	
- Possible bug to investigate: Go into a lobby while mods are selected and experience bugs
- Support opening practice mode while in a lobby so you can make sure it's synchronised
- Add a [[Song Queue]]
	This feature replaces the "Host rotation" setting that exists at the moment
	- Queued songs include the rate (and mods) at the time of selection
	- Host chooses if just the host or everyone can add to the queue
	- Host can add restrictions on what can be queued (keymodes, min and max length, min and max difficulty, number of songs queued by one person)
		- Selected filters automatically apply to level select when picking? Would be nifty
	- Host can veto queued songs
	- Host can reorder queued songs
	- Host can freely pick any song in the queue and put it at the top
	- Host can enable/disable a vote to skip songs
	- Queue automatically advances forward after a song is completed
	- Some kind of UI to see the history of the queue so the host can restore skipped or removed songs
- Support players playing the same song at different rates? Although I don't personally believe in this much
- [[Multiplayer Chat Commands]]
	- Rolling dice (for tournaments)
	- Commands to interact with queue quickly instead of using the UI