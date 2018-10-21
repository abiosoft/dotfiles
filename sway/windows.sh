#!/usr/bin/env bash

OUT=$(rofi -modi "drun,workspaces:python3 $HOME/dotfiles/sway/windows.py" -show-icons -show drun -location 1 -yoffset 30 -me-select-entry '' -me-accept-entry MousePrimary)

if [ "$OUT" != "" ]; then
    swaymsg workspace $(echo $OUT | awk -F' ' '{print $1}')
fi
