cd ../tools
dotnet tool uninstall -g YAVSRG.CLI
dotnet pack
dotnet tool install -g --add-source ./nupkg YAVSRG.CLI
cd ../scripts