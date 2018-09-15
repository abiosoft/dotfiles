#!/usr/bin/env bash

echo installing...
mkdir -p $HOME/.fonts
cp ./SF/*.otf $HOME/.fonts
cp ./fonts.conf $HOME/.fonts.conf

echo resetting cache...
fc-cache -fv
