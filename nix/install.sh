#!/usr/bin/env bash

# this script is designed to only work on Linux
# as macOS nix install requires user interaction.

set -ex

# verify provision
if [ -f "$HOME/.provision" ]; then exit 0; fi

# dotfiles, this is for VMs
rm -rf ~/dotfiles
cp -r /Users/abiola/dotfiles ~/dotfiles

# install nix
sh <(curl -L https://nixos.org/nix/install) --no-daemon
. ~/.nix-profile/etc/profile.d/nix.sh
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --update

# install home-manager
export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
nix-shell '<home-manager>' -A install
mkdir -p ~/.config/nixpkgs
ln -sf ~/dotfiles/nix/home.nix ~/.config/nixpkgs/home.nix
home-manager switch

# download vscode server for possible vscode remote
# bash ~/dotfiles/bin/dl-vscode-server.sh
touch ~/.provision
