#!/usr/bin/env bash
set -ex

# install homebrew
NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# add Homebrew to the login profile
(echo; echo 'eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"') >> ~/.profile
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

# clone dotfiles
git clone https://github.com/abiosoft/dotfiles ~/dotfiles --depth 1

# stow dotfiles
(cd ~/dotfiles && stow bat brew bin git neovim tmux zsh)

# install brew packages
brew bundle -v --file ~/.config/brew/Brewfile
