#!/usr/bin/env bash

set -ex

# verify provision
if [ -f "$HOME/.provision" ]; then exit 0; fi

# dotfiles, this is for VMs
# cp -r /Users/abiola/dotfiles ~/dotfiles

# install nix
cd
curl -L https://nixos.org/nix/install | sh
. ~/.nix-profile/etc/profile.d/nix.sh
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --update

# install home-manager
export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
nix-shell '<home-manager>' -A install
cp ~/dotfiles/nix/home.nix ~/.config/nixpkgs/home.nix
home-manager switch

# setup the rest of dotfiles, set distro accordingly
cd ~/dotfiles/ && distro=none ./setup.sh

# download vscode server for possible vscode remote
bash ~/dotfiles/bin/dl-vscode-server.sh
touch ~/.provision


