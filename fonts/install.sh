#!/usr/bin/env bash

echo installing...
mkdir -p $HOME/.fonts
cp ./SF/*.otf $HOME/.fonts
mkdir -p $HOME/.config/fontconfig/
cp ./fonts.conf $HOME/.config/fontconfig/fonts.conf

echo resetting cache...
fc-cache -fv
