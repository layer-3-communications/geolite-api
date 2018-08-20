#!/usr/bin/env bash

set -e

userAtHost='192.168.254.11'
# userAtHost="$1"
nix-build ../default.nix -j8
program=$(readlink -f result)

nix-copy-closure --to amartin@"$userAtHost" "$program"
ssh amartin@"$userAtHost" nix-env -i "$program"
