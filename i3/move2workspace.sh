#!/bin/bash

ID=$(($(i3-msg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))

if [ $1 = "rofi" ]; then
    WK=$(wmctrl -d | awk -F' ' '{ print $NF}' | rofi -dmenu -p workspace -mesg "select workspace to move to, or leave blank for new")
    if [ $? -eq 0]; then
        if [ ! -z "$WK" ] then 
            ID="$WK"
        fi
    fi
fi

i3-msg move container to workspace $ID
i3-msg workspace $ID


