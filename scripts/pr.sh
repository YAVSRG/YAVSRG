#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$REPO_ROOT"

chmod +x ./scripts/yavsrg.sh

# Build and test
dotnet build && dotnet test prelude/tests/Prelude.Tests.fsproj -- NUnit.ConsoleOut=0 && dotnet test online/tests/domain/Interlude.Web.Tests.Domain.fsproj -- NUnit.ConsoleOut=0 || (echo "Everything must build, all tests must pass" || exit 1)

# Formatting
if output="$(git status --porcelain 2>&1)"; then
    status=0
else
    status=$?
fi

if [[ "$status" -ne 0 || -n "$output" ]]; then
    if [[ -n "$output" ]]; then
        printf '%s\n' "$output"
    fi
    echo "All changes need to be committed or stashed before the auto-formatter runs."
    exit 1
fi

./scripts.yavsrg.sh format && git commit -a -m "chore: automatic formatting"
./scripts/yavsrg.sh locale_fix

echo ""
echo "All checks passed, code was auto-formatted! You're ready to open a PR"
exit 0