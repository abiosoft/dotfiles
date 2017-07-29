dotfiles
========
zsh, tmux and neovim configurations.

## Screenshots

### Dark theme

![Dark theme](https://github.com/abiosoft/dotfiles/blob/master/dark.png)

### Light theme

![Light theme](https://github.com/abiosoft/dotfiles/blob/master/light.png)

## ZSH

Install custom theme based on pure zsh theme.
```sh
cp abiola.zsh-theme ~/.oh-my-zsh/themes
```

Configure
```sh
ZSH_THEME="abiola"
```

## Tmux

```sh
cp tmux.conf ~/.tmux.conf
```

## Neovim

### Install

```sh
brew install neovim/neovim/neovim
```

### Neovim config

```sh
cp init.vim ~/.config/nvim/init.vim
```

### Tomorrow Night color

Install. Replace `tomorrow` with `tomorrow-night`, if you prefer the light theme.
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

Install autocomplete
```viml
Plug 'zchee/deoplete-go', { 'do': 'make'}
```

Configure
```viml
let g:go_auto_type_info = 1
let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode

set completeopt+=noinsert
set completeopt+=noselect
set completeopt-=preview
```

### Rust

Install plugins
```viml
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'
```

Configure
```viml
let g:rustfmt_autosave = 1
set hidden
let g:racer_cmd = $HOME."/.cargo/bin/racer"
let g:racer_experimental_completer = 1
au FileType rust nmap gd <Plug>(rust-def)
au FileType rust nmap gs <Plug>(rust-def-split)
au FileType rust nmap gx <Plug>(rust-def-vertical)
au FileType rust nmap <leader>gd <Plug>(rust-doc)
```

### Code commenting

```viml
noremap <leader>cc :Commentary<cr>
```

### Replace vim

```sh
alias vim=nvim
```
Make `sudo` use nvim as well.
```sh
alias sudo='sudo '
```

