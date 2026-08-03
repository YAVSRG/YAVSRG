#!/usr/bin/env bash

set -euo pipefail

cd ../prelude/tests
dotnet test

cd ../../online/tests/domain
dotnet test
