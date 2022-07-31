#!/usr/bin/env bash

if [ ! -z "$1" ]; then
  tmux switch -t "$1"
  exit $?
fi

fzf="fzf --layout=reverse --prompt='switch to window: ' --header='tmux windows' --margin=25% --padding=5% --border=sharp"
tmux new-window -n "[choose-window]" "tmux select-window -t \$(tmux lsw | $fzf | awk -F':' '{print \$1}')"

