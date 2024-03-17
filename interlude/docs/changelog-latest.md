0.7.19
====

Lots of new technical stuff and a big leap forward in pattern analysis!

# New score database
Scores are now saved in a real database file using SQLite instead of being a flat file

This has a few benefits

- WAY faster loading and saving
- Significantly more reliable against abnormal crashes like blue screening or power outage
- If you have SQLite browsing software you can now look at the data and make queries :)

The upgrade will be done automatically, afterwards you can delete your old score.json files

# Pattern analysis
Added a new "summary"/categorisation to all charts giving a rough idea of what's in it  
This category also comes with a breakdown of what the major components of the chart are and any relevant minor ones

The idea of this is to be less fine-grained than the details tab currently on level select but enough to be really game-changingly useful for finding what to play

With this update

- Pattern analysis is now turned on for everyone, not just a hidden feature in Advanced settings
- Group by Pattern on level select is now very good, please try it and give me feedback
- Pattern summary is displayed on that blank spot on the score screen

# Other changes
- Mass improvements to the layout of the codebase (particularly Prelude)

