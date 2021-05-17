#!/usr/bin/env bash

cat > "$HOME"/.tmux.conf <<EOF
# simply source the tmux config in my dotfiles
source-file ~/dotfiles/tmux/tmux.conf
EOF


