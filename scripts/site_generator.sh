cd ../interlude/docs && ./update_wiki_toc.sh && cd ../../site
dotnet fsi generate_site.fsx || bash