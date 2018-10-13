#!/usr/bin/env bash

echo installing neovim and deps...
sudo apt -y install neovim curl python-pip python3-pip ctags && \
    pip2 install --user neovim autopep8 pylint rope --upgrade && \
    pip3 install --user neovim autopep8 pylint repe --upgrade


