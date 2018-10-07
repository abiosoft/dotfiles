#!/usr/bin/env bash

[ "$1" = "window" ] && i3-msg mode "Move Window. arrow key: within visible workspaces. shift+arrow key: across all workspaces"
[ "$1" = "display" ] && i3-msg mode "Move to Monitor. arrow key: move window. shift+arrow key: move workspace"