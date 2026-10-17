#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$REPO_ROOT"

if output="$(dotnet run --project tools/YAVSRG.CLI.fsproj --no-build -- locale_check 2>&1)"; then
    status=0
else
    status=$?
fi

if [[ "$status" -ne 0 || -n "$output" ]]; then
    if [[ -n "$output" ]]; then
        printf '%s\n' "$output"
    fi
    echo "::error::Localisation check failed. Run `yavsrg locale_fix` to fix any issues."
    exit 1
fi
