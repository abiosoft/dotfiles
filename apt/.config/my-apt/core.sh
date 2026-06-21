#!/bin/bash
#
# Core Ubuntu/Debian packages installation script.
#
# For additional packages, create ~/.config/my-apt/packages.sh.
# A template exists at ~/.config/my-apt/packages.sample.sh.

set -e

# Core packages
CORE_PACKAGES=(
    # shell
    bat
    tmux
    neovim
    tree
    stow
    zsh

    # utils
    jq
    yq
    htop
    btop
    fzf
    watch
    ripgrep
    silversearcher-ag
    git
    git-delta
    socat

    # internet
    curl
)

# Load extra packages if they exist
EXTRA_PACKAGES=()
packages_file="$(dirname "$0")/packages.sh"
if [[ -f "$packages_file" ]]; then
    # shellcheck source=/dev/null
    source "$packages_file"
fi

# Install all packages in a single call
sudo apt update
sudo apt install -y "${CORE_PACKAGES[@]}" "${EXTRA_PACKAGES[@]}"

# oh-my-zsh
if ! (cd ~/.oh-my-zsh && git status) > /dev/null 2>&1; then
    git clone https://github.com/ohmyzsh/ohmyzsh --depth=1 ~/.oh-my-zsh
fi
