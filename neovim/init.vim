if exists('g:vscode')
    source ~/dotfiles/neovim/config/vscode.vim
else
    source ~/dotfiles/neovim/config/vimplug.vim
    source ~/dotfiles/neovim/config/general.vim
    source ~/dotfiles/neovim/config/coc.vim
endif
