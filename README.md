dotfiles
========

Dotfiles managed with Brew and Stow

![Screenshot](screenshots/screenshot.png)

## Prerequisite

Homebrew

```sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

## Installation

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

## Declarative brew packages

```sh
# edit ~/.config/brew/packages.rb to add extra packages
# run the brew-switch alias
brew-switch
```

