#!/usr/bin/env bash

TITLE="$1"
OPTIONS=$(echo -e "$2")
LINES=$(echo "$OPTIONS" | wc -l)
WIDTH=$(($(echo "$TITLE\n$OPTIONS" | wc -L) + 2))

MMODE="element selected.normal { background-color: @normal-background; text-color: @normal-foreground; }"
[ "$3" != "true" ] && [ -z "$POSITION" ] && MMODE="" && POSITION="-monitor -3"

echo "$OPTIONS" | rofi -dmenu -me-select-entry '' -me-accept-entry 'MousePrimary' -theme-str "#prompt { enabled: false; } #inputbar { children: [];} element alternate.normal { background-color: @normal-background; text-color: @normal-foreground; } $MMODE" -mesg "$TITLE" -lines $LINES -width "-$WIDTH" -font "SF Pro Display 11" $POSITION



