#!/usr/bin/env bash

set -e

# stable channel
NIXPKGS="nixpkgs/release-22.11"

# concat_packages <prefix> <packages ...>
concat_packages() (
    prefix="$1"

    shift
    list=("$@")

    packages=""
    for i in "${list[@]}"; do
        packages="${packages} ${prefix}${i}"
    done

    echo "$packages"
)

PACKAGES=""
EXTRA_FILE="$HOME/.config/nix/packages.sh"

. "$HOME/.config/nix/core.sh"
PACKAGES="$PACKAGES $(concat_packages "${NIXPKGS}#" "${core_packages[@]}")"

if [ -f "$EXTRA_FILE" ]; then
    . "$EXTRA_FILE" 
    PACKAGES="$PACKAGES $(concat_packages "${NIXPKGS}#" "${stable_packages[@]}")"
    PACKAGES="$PACKAGES $(concat_packages "nixpkgs#" "${unstable_packages[@]}")"
fi

nix profile remove '.*' 2> /dev/null && nix profile install $PACKAGES
