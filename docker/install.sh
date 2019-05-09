#!/usr/bin/env bash

docker volume create box_home

docker run --rm -d --name temp -v box_home:/home alpine tail -f /dev/null

docker cp ../. temp:/home/dotfiles
docker cp ../zsh/zshrc temp:/home/.zshrc

docker exec -it temp mkdir -p /home/.config/nvim
docker cp ../neovim/init.vim temp:/home/.config/nvim/init.vim

# ssh
docker cp $HOME/.ssh/. temp:/home/.ssh

# git config
docker cp $HOME/.gitconfig temp:/home/.gitconfig

docker rm -f temp

