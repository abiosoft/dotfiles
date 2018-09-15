#!/usr/bin/env bash


dirs=(
    "fonts"
    "neovim"
    "tmux"
    "zsh"
)

dirs+=( $desktop )

if [ -z "$distro" ]; then
    echo distro not specified
    exit 1
fi

if [ -z "$desktop" ]; then
    echo desktop not specified
    exit 1
fi

echo setting up for $distro with $desktop...

for dir in ${dirs[@]}; do
    echo setting up $dir...
    sh -c "cd $dir && ( ( [ -f deps-$distro.sh ] && ./deps-$distro.sh ) || echo no deps ) && ./install.sh"

    if [ $? -ne 0 ]; then
        exit 1
    fi

    echo done with $dir.
done

