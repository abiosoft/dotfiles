#!/usr/bin/env bash

ACTIVE=$(i3-msg -t get_workspaces | jq '.[] | select(.focused==true).output' | cut -d"\"" -f2)

if [ "$ACTIVE" = "$1" ]; then
    exit 0
fi

exit 1