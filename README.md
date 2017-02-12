dotfiles
========
My ZSH and Neovim configuration.

## ZSH

Install custom theme based on pure zsh theme.
```sh
cp abiola.zsh-theme ~/.oh-my-zsh/themes
```

Configure
```sh
ZSH_THEME="abiola"
```

## Neovim

### Install

```sh
brew install neovim
```

### Neovim config

```sh
cp init.vim ~/.config/nvim/init.vim
```

### Tomorrow Night color

Install
```sh
mkdir -p ~/.config/nvim/colors
cp tomorrow-night.vim ~/.config/nvim/colors
```

Configure
```viml
colorscheme tomorrow-night
```

### Python 3
Deoplete and some other plugins require python 3.

Install
```sh
brew install python
brew install python3
pip2 install neovim --upgrade
pip3 install neovim --upgrade
```

Configure
```viml
let g:python2_host_prog = '/usr/local/bin/python'
let g:python3_host_prog = '/usr/local/bin/python3'
```

### Ruby
```sh
gem install neovim
```

### Autocomplete

Install Deoplete
```viml
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
```

Configure
```viml
let g:deoplete#enable_at_startup = 1
```

### File Search

Install FZF
```viml
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
```

Configure. Remap to Ctrl-P
```viml
nnoremap <C-p> :FZF<CR>
```

### Go

```viml
let g:go_auto_type_info = 1
```

### Replace vim

```sh
alias vim=nvim
```

