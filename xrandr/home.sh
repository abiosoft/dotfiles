#!/usr/bin/env bash

xrandr --fb 7852x2880
xrandr --output DP-1-1 --scale-from 5120x2880
xrandr --output eDP-1-1 --scale-from 2732x1536 --pos 5120x0
gsettings set org.gnome.desktop.interface scaling-factor 2

# xrandr --output eDP-1-1 --scale-from 3840x2160
# double the resolution you want it to be on low dpi display
# then go back to display setting and set hidpi to 200%
#  e.g. xrandr --output eDP-1-1 --scale-from 2732x1536


# xrandr \
#     --output DP-1-1 --auto --pos 0x0 \
#     --output eDP-1-1 --auto --panning 3840x2160+3840+0 --right-of DP-1-1

# --dpi 192 --fb 5762x4920 \
# --mode 3840x2160
# --pos 3840x0


