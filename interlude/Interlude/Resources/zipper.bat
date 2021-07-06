:: SCRIPT TO ASSIST IN ZIPPING THE DEFAULT EMBEDDED GAME ASSETS
:: (I edit them extracted as a folder and zip them right before building)
:: before first use, extract the zips into the same directory (under same names) for editing

del default.zip
"C:\Program Files\7-Zip\7z" a -tzip default.zip .\default\*
del defaultBar.isk
"C:\Program Files\7-Zip\7z" a -tzip defaultBar.isk .\defaultBar\*
del defaultArrow.isk
"C:\Program Files\7-Zip\7z" a -tzip defaultArrow.isk .\defaultArrow\*
del defaultOrb.isk
"C:\Program Files\7-Zip\7z" a -tzip defaultOrb.isk .\defaultOrb\*