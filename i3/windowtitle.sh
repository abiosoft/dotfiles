#!/usr/bin/env bash

id=$(xprop -root | awk '/_NET_ACTIVE_WINDOW\(WINDOW\)/{print $NF}')
name=$(xprop -id $id | awk '/_NET_WM_NAME/{$1=$2="";print}' | cut -d'"' -f2)
title=$(wmctrl -l -x | grep ${id:2} | awk -F' ' '{print $3}' | awk -F'.' ' {print $2} ')

[ "$name" = "" ] && echo "no open window ..." && exit

name="$title - $name              ..."

echo ${name:0:18}
