#!/usr/bin/env bash

if [ ! -z "$1" ]; then
  tmux switch -t "$1"
  exit $?
fi

fzf="fzf --layout=reverse --prompt='switch to: ' --header='tmux sessions' --margin=25% --padding=5% --border=sharp"
tmux new-window -n "[choose-session]" "tmux switch -t \$(tmux ls | $fzf | awk -F':' '{print \$1}')"

