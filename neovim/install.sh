#!/usr/bin/env bash

mkdir -p $HOME/.config/nvim/colors
mkdir -p $HOME/.config/nvim/lua

echo installing nvim config...
cp tomorrow-night.vim tomorrow.vim $HOME/.config/nvim/colors
cp init.vim coc-settings.json $HOME/.config/nvim
ln -s $HOME/dotfiles/neovim/config/lsp.lua $HOME/.config/nvim/lua/lsp.lua

