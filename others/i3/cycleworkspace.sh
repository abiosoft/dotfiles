#!/bin/bash

[ -z "$1" ] && exit 1
[ -z "$MONITOR" ] && exit 1

if [ $1 = "up" ]; then
    CMD="head"
    DIR="prev"
elif [ "$1" = "down" ]; then
    CMD="tail"
    DIR="next"
fi

i3-msg focus output $MONITOR

LAST=$(i3-msg -t get_workspaces | jq ".[] | select(.output==\"$MONITOR\").name" | cut -d"\"" -f2 | $CMD -n1)
CURRENT=$(i3-msg -t get_workspaces | jq '.[] | select(.focused==true).name' | cut -d"\"" -f2)

[ "$CURRENT" = "$LAST" ] && exit 0

i3-msg workspace "${DIR}_on_output"

