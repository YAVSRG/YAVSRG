cd ../interlude/tools
dotnet pack
dotnet tool install -g --add-source ./nupkg Interlude.Tools