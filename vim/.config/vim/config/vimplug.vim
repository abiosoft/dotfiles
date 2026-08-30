vim9script

# Determine the standard Vim plugin paths
var plug_path = expand('~/.config/vim/autoload/plug.vim')
var plugged_dir = expand('~/.config/vim/plugged')

# Auto-install vim-plug if missing
if empty(glob(plug_path))
    silent execute '!curl -fLo ' .. plug_path .. ' --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    augroup VimPlugInstall
        autocmd!
        autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
    augroup END
endif

# Initialize vim-plug
plug#begin(plugged_dir)

# General Utilities
Plug 'Raimondi/delimitMate'
Plug 'Yggdroot/indentLine'
Plug 'ojroques/vim-oscyank'

# Git
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

# Language & Navigation
Plug 'fatih/vim-go'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

plug#end()

