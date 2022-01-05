:: SCRIPT FOR PACKAGING A FRESH INSTALL OF INTERLUDE, READY TO ZIP
:: just adjust output directory to where it gets built

@echo off
set output="C:\Users\percy\Desktop\Source\YAVSRG\Interlude\bin\Release\netcoreapp3.1\"
rmdir /Q /S "%output%../Clean"
xcopy /I "%output%*.dll" "%output%../Clean"
xcopy /I "%output%*.so" "%output%../Clean"
xcopy /I "%output%Interlude.exe" "%output%../Clean"
xcopy /I "%output%Interlude.runtimeconfig.json" "%output%../Clean"
xcopy /I "%output%Interlude.deps.json" "%output%../Clean"
xcopy /I /e "%output%runtimes" "%output%../Clean/runtimes"
xcopy /I /e "%output%Locale" "%output%../Clean/Locale"
xcopy /I /e "%output%Fonts" "%output%../Clean/Fonts"
set /p v="Enter Version No. "
"C:\Program Files\7-Zip\7z" a -tzip "%output%../Interlude.%v%.zip" "%output%../Clean/*"
rmdir /Q /S "%output%../Clean"
echo "DONE"
pause