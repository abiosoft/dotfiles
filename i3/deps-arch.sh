#!/usr/bin/env bash


sudo pacman -Sy --noconfirm rofi i3status i3-gaps dconf wmctrl i3blocks xdotool alsa-lib libpulse jsoncpp  wireless_tools libmpdclient caja jq yad
sudo pacman -Sy --noconfirm polybar || echo "cannot install polybar, install manually"

