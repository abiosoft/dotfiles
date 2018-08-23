#!/usr/bin/env bash

echo installing...
mkdir -p "$HOME/.config/gtk-3.0"
mkdir -p "$HOME/.themes"

cp ./gtk-3.0/gtk.css $HOME/.config/gtk-3.0
cp -R ./mytheme $HOME/.themes/mytheme
