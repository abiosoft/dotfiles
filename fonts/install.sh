#!/usr/bin/env bash

echo installing deps...
sudo pacman -Sy --noconfirm fontconfig
if [ $? -ne 0 ]; then
    exit 1
fi

echo installing...
mkdir -p $HOME/.fonts
cp ./SF/*.otf $HOME/.fonts
cp ./fonts.conf $HOME/.fonts.conf

echo resetting cache...
fc-cache -fv
