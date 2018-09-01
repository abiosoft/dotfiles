#!/usr/bin/env bash

which pacman > /dev/null
if [ $? -ne 0 ]; then
    echo "not an arch based distro"
    exit 1
fi

echo neovim and deps...
sudo pacman -Sy --noconfirm neovim python-pip python2-pip && \
    pip install --user neovim --upgrade && \
    pip2 install --user neovim --upgrade

if [ $? -ne 0 ]; then
    exit 1
fi

mkdir -p $HOME/.config/nvim/colors

echo installing nvim config...
cp tomorrow-night.vim tomorrow.vim $HOME/.config/nvim/colors
cp init.vim $HOME/.config/nvim

