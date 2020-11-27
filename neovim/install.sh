#!/usr/bin/env bash

mkdir -p $HOME/.config/nvim/colors

echo installing nvim config...
cp tomorrow-night.vim tomorrow.vim $HOME/.config/nvim/colors
cp coc-settings.json $HOME/.config/nvim

cat > ~/.config/nvim/init.vim <<EOF
" source the dotifle neovim config
source ~/dotfiles/neovim/init.vim
EOF

