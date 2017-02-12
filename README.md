dotfiles
========
My ZSH and Neovim configuration.

## ZSH

Install custom theme based on pure zsh theme.
```
cp abiola.zsh-theme .oh-my-zsh/themes
```

Configure
```
ZSH_THEME="abiola"
```

## Neovim

### Install

```
brew install neovim
```

### Neovim config

```
cp init.vim ~/.config/nvim/init.vim
```

### Tomorrow Night color

Install
```
mkdir -p ~/.config/nvim/colors
cp tomorrow-night.vim /.config/nvim/colors
```

Configure
```
colorscheme tomorrow-night
```

### Python 3
Deoplete and some other plugins require python 3.

Install
```
brew install python
brew install python3
pip2 install neovim --upgrade
pip3 install neovim --upgrade
```

Configure
```
let g:python2_host_prog = '/usr/local/bin/python'
let g:python3_host_prog = '/usr/local/bin/python3'
```

### Ruby
```
gem install neovim
```

### Autocomplete

Install Deoplete
```
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
```

Configure
```
let g:deoplete#enable_at_startup = 1
```

### File Search

Install FZF
```
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
```

Configure. Remap to Ctrl-P
```
nnoremap <C-p> :FZF<CR>
```

### Go

```
let g:go_auto_type_info = 1
```

### Replace vim

```
alias vim=nvim
```

