#!/usr/bin/env bash
id=$(xprop -root | awk '/_NET_ACTIVE_WINDOW\(WINDOW\)/{print $NF}')
name=$(xdotool getactivewindow getwindowname)
app=$(wmctrl -l -x | grep ${id:2} | awk -F' ' '{print $3}' | awk -F'.' ' {print $2} ')

[ "$name" = "" ] && echo "no open window" && exit

name="$app - $name"

echo ${name:0:100}
