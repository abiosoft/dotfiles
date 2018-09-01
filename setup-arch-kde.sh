#!/usr/bin/env bash

echo setting up for arch/arch based distro with KDE...

dirs=(
    "fonts"
    "kde"
    "neovim"
    "tmux"
    "zsh"
)

for dir in ${dirs[@]}; do
    echo $dir
    sh -c "cd $dir && ./install.sh"

    if [ $? -ne 0 ]; then
        exit 1
    fi
done

