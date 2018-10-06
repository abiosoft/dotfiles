#!/bin/bash

ID=$(($(i3-msg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))

if [ "$1" = "rofi" ]; then
    WKL="create new workspace"
    WK=$(printf "$WKL\n$(wmctrl -d | awk -F' ' '{ print $NF}')" | rofi -dmenu -p workspace)
    if [ $? -eq 0 ]; then
        if [ "$WK" != "$WKL" ]; then 
            ID="$WK"
        fi
    else
        exit 0
    fi
fi

i3-msg move container to workspace $ID
i3-msg workspace $ID


