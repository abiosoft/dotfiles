#!/usr/bin/env bash

echo installing neovim and deps...
sudo apt -y install neovim curl python-pip python3-pip && \
    pip2 install --user neovim --upgrade && \
    pip3 install --user neovim --upgrade


