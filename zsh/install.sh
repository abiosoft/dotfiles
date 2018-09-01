#!/usr/bin/env bash

echo installing zsh...
sudo pacman -Sy --noconfirm zsh pkgfile && sudo pkgfile --update
if [ $? -ne 0 ]; then
    exit 1
fi

echo installing oh-my-zsh...
sh oh-my-zsh.sh
if [ $? -ne 0 ]; then
    exit 1
fi

echo copying zsh configs
cp abiola.zsh-theme $HOME/.oh-my-zsh/themes
cp zshrc $HOME/.zshrc
