#!/bin/bash

ID=$(($(i3-msg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))
SEP="------"

OPTIONS=$(cat <<EOF
left
right
top
bottom
$SEP
move to workspace
EOF
)

workspace(){
    ID="$1"
    WKL="create new workspace"
    WK=$(printf "$WKL\n$(wmctrl -d | awk -F' ' '{ print $NF}')" | rofi -dmenu -p "move to workspace" -location 2 -yoffset 25 -width 400)
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
    i3-msg move container to workspace $1
    i3-msg workspace $1
}

if [ "$1" = "rofi" ]; then
    MOVE=$(printf "$OPTIONS" | rofi -dmenu -p "move window" -location 2 -yoffset 25 -width 400) 
   
    if [ $? -eq 0 ]; then
        if [ "$MOVE" == "move to workspace" ]; then 
            workspace "$ID"
        elif [ "$MOVE" = "$SEP" ]; then
            exit 0
        else
            i3-msg move "$MOVE"
        fi
    else
        exit 0
    fi
else
    move $ID
fi

