dotfiles
========

Dotfiles managed with Stow for Nix or Brew

![Screenshot](screenshots/screenshot.png)

## Nix

### Prerequisite

Install Nix.

```sh
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

### Installation

`cd` into repository and setup dotfiles

```
nix run nixpkgs#stow bat bin git ideavim neovim nix tmux zsh
```

Pin the current nixpkgs version to avoid repetitive registry downloads

```
nix registry pin nixpkgs

# to update the pinned nix version later on
#   nix flake update path:$HOME/dotfiles/nix/.config/nix/
```

Install packages.

```
nix profile install path:$HOME/dotfiles/nix/.config/nix/
```

### Declarative Nix packages

```sh
# create a copy of sample packages file
cp ~/.config/nix/packages.sample.nix ~/.config/nix/packages.nix

# edit packages file to add extra packages
vim ~/.config/nix/packages.nix

# run the `nix-switch` alias
nix-switch
```

## Brew

### Prerequisite

Homebrew

```sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

### Installation

Install Stow

```
brew install stow
```

Setup dotfiles

```
stow bat brew bin git ideavim neovim tmux zsh
```

Install packages

```
brew bundle -v --file ~/.config/brew/Brewfile
```

### Declarative Brew packages

```sh
# create a copy of the sample packages file
cp ~/.config/brew/packages.sample.rb ~/.config/brew/packages.rb

# edit packages file to add extra packages
vim ~/.config/brew/packages.rb

# run the `brew-switch` alias
brew-switch
```

