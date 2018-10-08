#!/usr/bin/env bash

# MONITOR, PRINT

killpid(){
    kill -9 $1 > /dev/null
    rm $2 > /dev/null
}

print_title(){
    NAME=$(xdotool getactivewindow getwindowname)
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
}

watch_win(){
    # watch window
    while read line; do
        echo $1 $2
        print_title
    done &< <(tail --pid $1 -f $2 &)
}

watch_active(){
    PID=""
    tempfile=""

    while read line; do
        # only window title needs monitoring
        # others can return here
        [ ! -z "$PRINT" ] && print_title && continue

        # active window has changed
        # kill current window monitor
        killpid $PID $tmpfile || true

        ID=$(echo $line | awk -F' ' '{ print $NF }')

        # wait 5 secs if no active window befor checking again
        [ -z "$ID" ] && sleep 5 && continue


        # don't wait for it.
        tmpfile=$(mktemp)
        xprop -spy -id $ID _NET_WM_NAME 2> /dev/null > $tmpfile &
        PID=$!
        trap "killpid $PID $tmpfile" INT EXIT ERROR
        tail --pid $PID -f $tmpfile | while read line; do print_title; done &

    done < <(xprop -spy -root _NET_ACTIVE_WINDOW)
}

watch_active

