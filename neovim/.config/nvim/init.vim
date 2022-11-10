if exists('g:vscode')
    source ~/.config/nvim/config/vscode.vim
else
    source ~/.config/nvim/config/vimplug.vim
    source ~/.config/nvim/config/general.vim
    source ~/.config/nvim/config/coc.vim
    source ~/.config/nvim/config/fugitive.vim
    source ~/.config/nvim/config/go.vim
    source ~/.config/nvim/config/hcl.vim
    source ~/.config/nvim/config/fzf.vim
    source ~/.config/nvim/config/theme.vim
    source ~/.config/nvim/config/viminspector.vim
endif

