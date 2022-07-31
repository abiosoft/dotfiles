#!/bin/bash

ID=$(($(swaymsg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))
SEP="------"

OPTIONS=$(cat <<EOF
  left 
  right
  up
  down
$SEP
move to workspace
EOF
)

workspace(){
    ID="$1"
    WKL="create new workspace"
    WK=$(printf "$WKL\n$(wmctrl -d | awk -F' ' '{ print $NF}')" | rofi -dmenu -p "move to workspace" -location 2 -yoffset 30 -width 400 -i)
    if [ $? -eq 0 ]; then
        if [ "$WK" != "$WKL" ]; then
            ID="$WK"
        fi
    else
        exit 0
    fi

    move $ID
}

move(){
    swaymsg move container to workspace $1
    swaymsg workspace $1
}

if [ "$1" = "rofi" ]; then
    MOVE=$(printf "$OPTIONS" | rofi -dmenu -p "move window" -location 2 -yoffset 30 -width 400 -i)
    if [ $? -eq 0 ]; then
        if [ "$MOVE" == "move to workspace" ]; then
            workspace "$ID"
        elif [ "$MOVE" = "$SEP" ]; then
            exit 0
        else
            MOVE=$(echo "$MOVE" | awk -F' ' '{ $1=""; print $0 }')
            swaymsg move "$MOVE"
        fi
    else
        exit 0
    fi
else
    move $ID
fi

