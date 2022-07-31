#!/usr/bin/env bash

[ "$1" = "window" ] && i3-msg mode "Move Window. left right up down: across visible workspaces. shift+left right: across all workspaces. shift+up: new workspace"
[ "$1" = "display" ] && i3-msg mode "Move to Monitor. left right up down: move window. shift+left right up down: move workspace"
