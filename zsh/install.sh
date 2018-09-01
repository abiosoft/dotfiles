#!/usr/bin/env bash

echo installing zsh...
sudo pacman -Sy --noconfirm zsh pkgfile && sudo pkgfile --update
if [ $? -ne 0 ]; then
    exit 1
fi

echo installing oh-my-zsh...
sh -c "$(wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
if [ $? -ne 0 ]; then
    exit 1
fi

cp abiola.zsh-theme $HOME/.oh-my-zsh/themes
cp zshrc $HOME/.zshrc
