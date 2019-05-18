#!/bin/bash

ID=$(($(swaymsg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))
swaymsg workspace $ID
[ -z "$1" ] || swaymsg move workspace to output $1


