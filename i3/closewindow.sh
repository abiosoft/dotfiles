#!/usr/bin/env bash

TITLE=$(xdotool getactivewindow getwindowname)
MESSAGE="Are you sure you want to close $TITLE?"

OPTIONS=$(cat <<EOF
Yes
No
EOF
)

A=$(printf "$OPTIONS" | rofi -dmenu -p close -location 3 -mesg "$MESSAGE" -yoffset 25 -lines 3 -width 400 -i)
if [ $? -ne 0]; then
    exit 0
fi

if [ "$A" = "Yes" ]; then
    i3-msg kill
fi

