#!/usr/bin/env bash

set -euo pipefail

cd ../tools
dotnet tool uninstall -g YAVSRG.CLI || true
dotnet pack
dotnet tool install -g --add-source ./nupkg YAVSRG.CLI
