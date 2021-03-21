:: SCRIPT FOR PACKAGING A FRESH INSTALL OF INTERLUDE, READY TO ZIP
:: just copy it to the output directory and run

@echo off
set output = "C:/Users/percy/Desktop/Source/YAVSRG/Interlude/bin/Release/"
rmdir /s "%output%../Clean"
xcopy /I "%output%*.dll" "%output%../Clean"
xcopy /I "%output%Akrobat-Black.otf" "%output%../Clean"
xcopy /I "%output%Interlude.exe" "%output%../Clean"
xcopy /I "%output%Interlude.runtimeconfig.json" "%output%../Clean"
xcopy /I "%output%Interlude.deps.json" "%output%../Clean"
xcopy /I /e "%output%runtimes" "%output%../Clean/runtimes"
xcopy /I /e "%output%Locale" "%output%../Clean/Locale"
echo "DONE"
pause