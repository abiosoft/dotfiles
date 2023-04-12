dotfiles
========

Dotfiles managed with Stow for Nix or Brew

![Screenshot](screenshots/screenshot.png)

## Nix

### Prerequisite

Nix

```sh
export NIX_VERSION=2.15.0
sh <(curl -L https://releases.nixos.org/nix/nix-${NIX_VERSION}/install) --daemon
```

### Installation

Install Stow

```
nix-shell -p stow
```

Setup dotfiles

```
stow aerc bat bin git ideavim neovim nix tmux zsh
```

Install packages

```
nix-env -if ~/.config/nix/core.nix
```
### Adding unstable channel

```
nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs-unstable
nix-channel --update
```

### Declarative Nix packages

**NOTE**: only works in multi-user mode, running `nix-env -irf ...` in single user mode would remove and break `nix`.

```sh
# edit ~/.config/nix/packages.nix to add extra packages
nix-env -irf ~/.config/nix/core.nix
# or run the `nix-switch` alias
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
stow aerc bat brew bin git ideavim neovim tmux zsh
```

Install packages

```
brew bundle -v --file ~/.config/brew/Brewfile
```

### Declarative Brew packages

```sh
# edit ~/.config/brew/packages.rb to add extra packages
# run the brew-switch alias
brew-switch
```

