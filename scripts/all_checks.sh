cd ..

wget -q https://api.yavsrg.net/health -O - | grep "Everything will be ok." || echo "API health check failed!"

dotnet build -v q || echo "Build failed!"

fantomas --check . || echo "Some files need formatting!"

cd interlude/tools/bin/Debug/net8.0
dotnet run --project ../../.. -- check_locale | grep . && echo "Some locale keys need updating!"

echo "Checks complete"