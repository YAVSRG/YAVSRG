cd ../interlude/docs && ./update_wiki_toc.sh && cd ../..
cd interlude/tools && dotnet build --configuration Debug && cd bin/Debug/net8.0 && clear && dotnet run --project ../../.. -- publish
cd ../../../../..
./generate_site.sh
git add site
git commit -m "📘 Update site changelog/wiki"
git push