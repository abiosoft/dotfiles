#!/bin/sh
xrandr --output HDMI-2 --off --output HDMI-1 --off --output DP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output eDP-1 --mode 1920x1080 --pos 1920x0 --rotate normal --output DP-2 --off
xrandr --output eDP-1 --scale .7x.7

