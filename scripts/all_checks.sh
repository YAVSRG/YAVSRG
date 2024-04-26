cd ..
curl https://api.yavsrg.net/health | grep "Everything will be ok." || echo "API health check failed!"
dotnet build -v q || echo "Build failed!"
fantomas --check . || echo "Some files need formatting!"
yavsrg locale_check | grep . && echo "Some locale keys need updating!"
echo "Checks complete"