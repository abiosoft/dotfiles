#!/usr/bin/env bash

mkdir -p $HOME/.config/gtk-3.0

echo installing gtk settings...
cp gtk-settings.ini $HOME/.config/gtk-3.0/settings.ini

echo installing plasma template...
mkdir -p /usr/share/plasma/layout-templates/
sudo cp -R com.abiosoft.desktop.abiPanel /usr/share/plasma/layout-templates/

echo disabling launcher meta key...
kwriteconfig5 --file kwinrc --group ModifierOnlyShortcuts --key Meta ""

echo installing kscreen for screen settings persistence..
which pacman > /dev/null
if [ $? -eq 0 ]; then
    sudo pacman -Sy --noconfirm kscreen
fi

