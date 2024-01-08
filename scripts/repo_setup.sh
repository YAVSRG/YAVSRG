cd ..
git submodule update --init libraries/Percyqaz.Common
git submodule update --init libraries/Percyqaz.Data
git submodule update --init libraries/Percyqaz.Shell
cd interlude
git clone https://github.com/YAVSRG/Interlude.Assets.git assets || echo "Couldn't clone assets folder because you don't have access"