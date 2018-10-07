#!/usr/bin/env bash

# MONITOR, PRINT

xprop -spy -root _NET_ACTIVE_WINDOW | while read; do
    NAME=$(xdotool getactivewindow getwindowname 2> /dev/null)
    [ $? -ne 0 ] && NAME=""
    bash $HOME/.config/i3/activemonitor.sh $MONITOR
    if [ $? -eq 0 ] && [ "$NAME" != "" ]; then
        if [ "$PRINT" = "" ]; then
            echo $NAME
        else
            echo $PRINT
        fi
    else
        echo ""
    fi
done
