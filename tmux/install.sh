#!/usr/bin/env bash

echo installing tmux...
sudo pacman -Sy --noconfirm tmux
if [ $? -ne 0 ]; then
    exit 1
fi

cp tmux.conf $HOME/.tmux.conf

