#!/usr/bin/env bash

mkdir -p $HOME/.config/gtk-3.0

echo installing gtk settings...
cp gtk-settings.ini $HOME/.config/gtk-3.0/settings.ini

echo installing plasma template...
sudo cp -R org.manjaro.desktop.maiaPanel /usr/share/plasma/layout-templates/
