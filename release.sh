cd tools
dotnet build --configuration Debug && cd bin/Debug/net7.0 && clear && dotnet run --project ../../.. -- publish
cd ..
./generate_site.sh
./push.sh