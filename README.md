dotfiles
========

Dotfiles managed with Stow for Brew

![Screenshot](screenshots/screenshot.png)

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

