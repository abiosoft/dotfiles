#!/usr/bin/env bash

echo disable gnome csd
dconf write /org/gnome/desktop/wm/preferences/button-layout "'menu'"

echo copying sway configs
mkdir -p $HOME/.config/sway
cp sway/config ~/.config/sway
cp sway/windows.py ~/.config/sway/

mkdir -p $HOME/bin
cp sway/kblayout.sh ~/bin/
