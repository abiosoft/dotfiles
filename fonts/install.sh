#!/usr/bin/env bash

echo installing...
mkdir -p $HOME/.local/share/fonts
cp ./SF/*.otf $HOME/.local/share/fonts
# uncomment the following for arch linux
# mkdir -p $HOME/.config/fontconfig/
# cp ./fonts.conf $HOME/.config/fontconfig/fonts.conf

echo resetting cache...
fc-cache -fv
