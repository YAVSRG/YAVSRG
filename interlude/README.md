# **YAVSRG: Interlude**
### Yet Another Vertical Scrolling Rhythm Game

YAVSRG (Yet Another Vertical Scrolling Rhythm Game) is a rhythm game written in C# designed for 3-10k with the intent of combining the best features from other rhythm games and to create a space that can be used equally casually and competitively.

### Why?

osu!mania's difficulty rating and ranking system is very poor and does not evaluate skill properly. It rewards spamming buttons and playing only specific viable maps to gain ranks.
Etterna is too heavily geared towards competitive play with its harsh miss mechanics and clunky user interface.
The osu!mania map editor has only the bare minimum function, while editors for Stepmania have more features but don't support my workflow

I plan to have the best of both worlds with regards to charting features. Mines are retained from stepmania and the functionality of LNs and SVs will be retained from osu, this will allow for a significantly more diverse charting lineup + all files will eventually be converted to the .YAV format, where files can have both mines and svs simultaneously.

YAV will be an extremely versatile client, supporting all styles of play with modifiers for competitive play (rates, screen covers) and for casual play (chord cohesion etc.) Loading of the .osu and .sm filetypes is supported. Some features YAV will have that other rhythm games do not have include note colors based on chords and similar reading modifiers.

YAV will also contain a custom, automatic difficulty calculator. Physical and Technical. Physical is simply how physically strenuous the chart is, while technical is how difficult the chart is from a technical perspective (polyrhythms, difficult to read patterns, misleading patterning etc.) YAV also uses it's own scoring algorithms, derived as a means of balancing both osu and wife scoring, players are punished more for being inaccurate than missing.

A fully functional map editor ingame is planned.
