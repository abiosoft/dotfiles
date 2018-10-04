#!/usr/bin/env bash


echo installing gtk settings...
mkdir -p $HOME/.config/gtk-3.0
cp gtk-settings.ini $HOME/.config/gtk-3.0/settings.ini

echo setting up i3...
mkdir -p $HOME/.config/i3
cp config *.sh $HOME/.config/i3/config
cp wallpaper.png $HOME/Pictures/wallpaper.png
cp Xresources $HOME/.Xresources

echo setting up i3status...
mkdir -p $HOME/.config/i3status
cp i3status/config $HOME/.config/i3status/config
