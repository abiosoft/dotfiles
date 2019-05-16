#!/usr/bin/env bash

echo disable gnome csd...
dconf write /org/gnome/desktop/wm/preferences/button-layout "'menu'"

echo copying sway configs...
mkdir -p $HOME/.config/sway
cp ./config ~/.config/sway
cp ./windows.py ~/.config/sway/

mkdir -p $HOME/bin
cp ./kblayout.sh ~/bin/


echo copying other configs...
echo installing gtk settings...
mkdir -p $HOME/.config/gtk-3.0
cp ../i3/gtk-settings.ini $HOME/.config/gtk-3.0/settings.ini

echo setting up i3...
mkdir -p $HOME/.config/i3
cp ../i3/config *.sh *.py $HOME/.config/i3
cp ../i3/Xresources $HOME/.Xresources

echo setting up rofi...
mkdir -p $HOME/.config/rofi
cp ../i3/rofi/config $HOME/.config/rofi/config
mkdir -p $HOME/.local/share/rofi/themes
cp ../i3/rofi/tomorrow-night.rasi $HOME/.local/share/rofi/themes

# gestures
git clone https://github.com/bulletmark/libinput-gestures $HOME/libinput-gestures
grep -E '^input:' /usr/lib/group | sudo tee -a /etc/group
echo "adding $USER to input group"
sudo usermod -a -G input $USER

