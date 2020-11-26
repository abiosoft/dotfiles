if exists('g:vscode')
    source ~/dotfiles/neovim/config/vscode.vim
else
    source ~/dotfiles/neovim/config/vimplug.vim
    source ~/dotfiles/neovim/config/general.vim
    source ~/dotfiles/neovim/config/coc.vim
    source ~/dotfiles/neovim/config/fugitive.vim
    source ~/dotfiles/neovim/config/go.vim
    source ~/dotfiles/neovim/config/fzf.vim
    source ~/dotfiles/neovim/config/theme.vim
endif
