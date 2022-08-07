@echo off
set build="C:\Users\percy\Desktop\Source\YAVSRG\Interlude\src\Interlude.fsproj"
dotnet build -c Debug "%build%"
"C:\Users\percy\Desktop\Source\YAVSRG\Interlude\src\bin\Debug\netcoreapp3.1\Interlude.exe"