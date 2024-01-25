cd ../interlude/tools
dotnet build --configuration Debug -v q
cd bin/Debug/net8.0
clear
dotnet run --project ../../.. -- publish
cd ../../../../../scripts
./site_generator.sh
git add ../site
git commit -m "📘 Update site changelog & wiki"
git push