cd ../tools
dotnet pack
dotnet tool install -g --add-source ./nupkg YAVSRG.CLI
cd ../scripts