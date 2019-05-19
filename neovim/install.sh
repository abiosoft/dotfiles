#!/usr/bin/env bash

mkdir -p $HOME/.config/nvim/colors

echo installing nvim config...
cp tomorrow-night.vim tomorrow.vim $HOME/.config/nvim/colors
cp init.vim coc-settings.json $HOME/.config/nvim

