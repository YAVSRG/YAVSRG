:: SCRIPT FOR PACKAGING A FRESH INSTALL OF INTERLUDE, READY TO ZIP
:: just adjust output directory to where it gets built

@echo off
set build="C:\Users\percy\Desktop\Source\YAVSRG\Interlude\src\Interlude.fsproj"
echo -- BUILDING --
dotnet build -c Release "%build%"
echo -- BUILT --
set output="C:\Users\percy\Desktop\Source\YAVSRG\Interlude\src\bin\Release\netcoreapp3.1\"
rmdir /Q /S "%output%../Clean"
xcopy /I "%output%*.dll" "%output%../Clean"
xcopy /I "%output%*.so" "%output%../Clean"
xcopy /I "%output%Interlude.exe" "%output%../Clean"
xcopy /I "%output%Interlude.runtimeconfig.json" "%output%../Clean"
xcopy /I "%output%Interlude.deps.json" "%output%../Clean"
xcopy /I /e "%output%runtimes" "%output%../Clean/runtimes"
xcopy /I /e "%output%Locale" "%output%../Clean/Locale"
"C:\Program Files\7-Zip\7z" a -tzip "%output%../Interlude.%1.zip" "%output%../Clean/*"
rmdir /Q /S "%output%../Clean"
echo -- DONE --