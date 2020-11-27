#!/usr/bin/env bash

mkdir -p $HOME/.config/nvim/colors
mkdir -p $HOME/.vim/colors

echo installing nvim config...
cp tomorrow-night.vim tomorrow.vim $HOME/.config/nvim/colors
cp tomorrow-night.vim tomorrow.vim $HOME/.vim/colors
cp coc-settings.json $HOME/.config/nvim
cp coc-settings.json $HOME/.vim

cat > ~/.config/nvim/init.vim <<EOF
" source the dotifle neovim config
source ~/dotfiles/neovim/init.vim
EOF

cat > ~/.vimrc <<EOF
" source the dotifle neovim config
source ~/dotfiles/neovim/init.vim
EOF

