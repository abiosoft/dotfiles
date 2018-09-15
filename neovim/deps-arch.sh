#!/usr/bin/env bash

echo installing neovim and deps...
sudo pacman -Sy --noconfirm neovim python-pip python2-pip && \
    pip install --user neovim --upgrade && \
    pip2 install --user neovim --upgrade


