#!/bin/bash

ID=$(($(i3-msg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))
i3-msg workspace $ID
[ -z "$1" ] || i3-msg move workspace to output $1


