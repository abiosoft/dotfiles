#!/usr/bin/env bash

echo installing neovim and deps...
sudo dnf -y install curl neovim python-pip python3-pip ctags && \
    pip2 install --user neovim autopep8 pylint --upgrade && \
    pip3 install --user neovim autopep8 pylint --upgrade


