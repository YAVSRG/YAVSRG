cd ../interlude/tools && dotnet build --configuration Debug && cd bin/Debug/net7.0 && clear && dotnet run --project ../../.. -- publish
cd ../../scripts
./generate_site.sh
git add site
git commit -m "📘 Update site changelog & wiki"
git push