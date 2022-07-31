#!/usr/bin/env bash


echo installing gtk settings...
mkdir -p $HOME/.config/gtk-3.0
cp gtk-settings.ini $HOME/.config/gtk-3.0/settings.ini

echo setting up i3...
mkdir -p $HOME/.config/i3
cp config *.sh *.py $HOME/.config/i3
cp wallpaper.png $HOME/Pictures/wallpaper.png
cp Xresources $HOME/.Xresources

echo setting up i3status...
mkdir -p $HOME/.config/i3status
cp i3status/config $HOME/.config/i3status/config

echo setting up rofi...
mkdir -p $HOME/.config/rofi
cp rofi/config $HOME/.config/rofi/config
mkdir -p $HOME/.local/share/rofi/themes
cp rofi/tomorrow-night.rasi $HOME/.local/share/rofi/themes

echo setting up i3 in mate...
# remove close button
dconf write /org/mate/desktop/interface/gtk-decoration-layout "'menu'"
# set i3 as window manager
dconf write /org/mate/desktop/session/required-components/windowmanager "'i3'"
# disable desktop
dconf write /org/mate/desktop/background/show-desktop-icons "false"
# remove panels from components
dconf write /org/mate/desktop/session/required-components-list "['windowmanager', 'filemanager', 'dock']"

echo setting up polybar ...
mkdir -p $HOME/.config/polybar
cp polybar/* $HOME/.config/polybar

