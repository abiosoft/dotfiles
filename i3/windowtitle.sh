#!/usr/bin/env bash

# This is used with polybar to print current window title
# echo current active window title or an empty line if there is none
# Also, only prints title when the window is on the display output that matches $MONITOR
# to improve user experience of having wrong window title on a display.

killpid(){
    kill -9 $1 > /dev/null
    rm $2 > /dev/null
}

print_title(){
    # get current monitor width to deduce max char length for title.
    # assuming 20px per char which works fine.
    WIDTH=$(polybar --list-monitors | grep $MONITOR | awk -F': ' '{ print $2 }' | awk -F'x' '{print $1}')
    SIZE=$((WIDTH / 20))

    NAME=$(xdotool getactivewindow getwindowname)
    [ $? -ne 0 ] && NAME="" # ensuring name is not a whitespaced string
    bash $HOME/.config/i3/activemonitor.sh $MONITOR
    if [ $? -eq 0 ] && [ "$NAME" != "" ]; then
        echo ${NAME:0:$SIZE}
    else
        echo ""
    fi
}

watch_win(){
    PID=""
    tempfile=""

    while read line; do
        # active window has changed
        # kill current window monitor
        # we get an error on first iteration, not an issue
        killpid $PID $tmpfile || true

        # clear trap, process killed
        # this is to play it safe, there is a rare chance the terminated
        # pid is reassigned and wrong process could be terminated.
        trap -

        # clear any existing title
        # hack to clear title in case the only active window is closed
        echo ""

        # get active window ID
        ID=$(echo $line | awk -F' ' '{ print $NF }')

        # wait 5 secs if no active window before checking again
        [ -z "$ID" ] && sleep 5 && continue


        # monitor window using ID
        # to be able to get pid of window monitor process,
        # start in background and direct output to a file
        # tail the log file instead
        tmpfile=$(mktemp)
        xprop -spy -id $ID _NET_WM_NAME 2> /dev/null > $tmpfile &
        PID=$!
        tail --pid $PID -f $tmpfile | while read line; do print_title; done &

        # in case of the last iteration that hasn't killed the loop
        trap "killpid $PID $tmpfile" INT EXIT ERROR

    done < <(xprop -spy -root _NET_ACTIVE_WINDOW)
}

watch_win

