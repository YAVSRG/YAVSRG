#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$REPO_ROOT"

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
    echo "Localisation check failed! Run './scripts/yavsrg.sh locale_fix' to interactively fix any issues."
    exit 1
fi

# Formatting
if output="$(dotnet fantomas --check . 2>&1)"; then
    status=0
else
    status=$?
fi

if [[ "$status" -ne 0 || -n "$output" ]]; then
    if [[ -n "$output" ]]; then
        printf '%s\n' "$output"
    fi
    echo "Formatting check failed! Run './scripts/yavsrg.sh format' to reformat files"
    exit 1
fi

echo ""
echo "All checks passed!"
exit 0