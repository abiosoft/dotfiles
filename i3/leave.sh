#!/usr/bin/env bash

OPTIONS=$(cat <<EOF
Lock Screen
Logout
------
Shutdown
EOF
)

A=$(printf "$OPTIONS" | rofi -dmenu -p leave -location 3 -yoffset 25 -lines 5 -width 400 -i)
if [ $? -ne 0]; then
    exit 0
fi

if [ "$A" = "Lock Screen" ]; then
    xdotool key ctrl+alt+l
elif [ "$A" = "Logout" ]; then
    mate-session-save --logout-dialog
elif [ "$A" = "Shutdown" ]; then
    mate-session-save --shutdown-dialog
fi

