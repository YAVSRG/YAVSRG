git checkout main
git pull
git fetch --tags
git checkout $(git describe --tags "$(git rev-list --tags --max-count=1)")
chmod +x repo_setup.sh
./repo_setup.sh
chmod +x interlude_run.sh
./interlude_run.sh