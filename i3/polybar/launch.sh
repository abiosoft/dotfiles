#!/usr/bin/env bash

# Terminate already running bar instances
killall -q -9 polybar

pkill -f windowtitle.sh

# window title monitor script
LOGFILE="$HOME/.config/i3/.windowtitle.log"
echo > $LOGFILE # truncate
bash $HOME/.config/i3/windowtitle.sh watch > "$LOGFILE" &

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
PRIMARY=$(xrandr | grep primary | awk -F' ' '{print $1}')
for m in $(polybar --list-monitors | cut -d":" -f1); do
    POS=""
    if [ "$m" = "$PRIMARY" ]; then
        POS="right"
    fi
    PRIMARY=$POS MONITOR=$m TITLEFILE=$LOGFILE polybar --reload example &
done
