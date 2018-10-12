#!/usr/bin/env bash

echo installing neovim and deps...
sudo pacman -Sy --noconfirm curl neovim python-pip python2-pip ctags && \
    pip install --user neovim autopep8 pylint --upgrade && \
    pip2 install --user neovim autopep8 pylint --upgrade


