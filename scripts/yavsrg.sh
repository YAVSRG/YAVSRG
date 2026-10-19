#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$REPO_ROOT"

if [[ "${1-}" == "update_cli" ]]; then
    cd tools
	dotnet tool uninstall -g YAVSRG.CLI || true
	dotnet pack
	dotnet tool install -g --add-source ./nupkg YAVSRG.CLI
    exit
fi

if command -v yavsrg >/dev/null 2>&1; then
    exec yavsrg "$@"
else
    cd tools
    exec dotnet run -- "$@"
fi
