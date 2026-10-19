#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$REPO_ROOT"

chmod +x ./scripts/yavsrg.sh

# Build and test
dotnet build && dotnet test prelude/tests/Prelude.Tests.fsproj -- NUnit.ConsoleOut=0 && dotnet test online/tests/domain/Interlude.Web.Tests.Domain.fsproj -- NUnit.ConsoleOut=0 || (echo "Everything must build, all tests must pass" || exit 1)

# Localisation
if output="$(./scripts/yavsrg.sh locale_check 2>&1)"; then
    status=0
else
    status=$?
fi

if [[ "$status" -ne 0 || -n "$output" ]]; then
    if [[ -n "$output" ]]; then
        printf '%s\n' "$output"
    fi
    echo "Localisation check failed! Fix this by running './scripts/yavsrg.sh locale_fix' OR './scripts/pr.sh'"
    exit 1
fi

# Formatting
dotnet tool restore
if output="$(dotnet fantomas --check . 2>&1)"; then
    status=0
else
    status=$?
fi

if [[ "$status" -ne 0 || -n "$output" ]]; then
    if [[ -n "$output" ]]; then
        printf '%s\n' "$output"
    fi
    echo "Formatting check failed! Fix this by running './scripts/yavsrg.sh format' OR './scripts/pr.sh'"
    exit 1
fi

echo ""
echo "All checks passed!"
exit 0