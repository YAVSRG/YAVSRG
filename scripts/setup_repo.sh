#!/usr/bin/env bash

set -euo pipefail

cd ..
git submodule update --init \
    libraries/Percyqaz.Common \
    libraries/Percyqaz.Data \
    libraries/Percyqaz.Shell
