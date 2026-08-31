vim9script

# ==============================================================================
# Basic Options
# ==============================================================================
set backspace=indent,eol,start

# Indentation Defaults
set tabstop=4
set softtabstop=0
set shiftwidth=4
set expandtab

# Navigation & UI State
set hidden
set scrolloff=3
set cursorline
set autoread
set mouse=a

# Search Settings
set hlsearch
set incsearch
set ignorecase
set smartcase

# Disable Swap Files and Backups
set nobackup
set noswapfile

# Interface & Display
set showcmd
set laststatus=2
set modeline
set modelines=10
set title
set titleold="Terminal"
set titlestring=%F
set statusline=%F%m%r%h%w%=(%{&ff}/%Y)\ (line\ %l\/%L,\ col\ %c)\

# Visuals & Color Support
syntax on
set ruler
set number
set relativenumber
set termguicolors

# Native Grep (Uses ripgrep if installed)
if executable('rg')
    set grepprg=rg\ --vimgrep
    set grepformat=%f:%l:%c:%m
endif

# Built-in Vim9 Commenting
packadd! comment
# Toggle comment on current line (Normal mode)
nmap <leader>cc gcc
# Toggle comment on selected block (Visual mode)
xmap <leader>cc gc

# Mode-based Cursor Shapes (Native Escape Sequences)
set ttimeout
set ttimeoutlen=10
&t_SI = "\e[6 q"
&t_EI = "\e[2 q"

# Clipboard
if has('unnamedplus')
    set clipboard=unnamed,unnamedplus
endif

# ==============================================================================
# Built-in File Tree (Netrw) Configuration
# ==============================================================================
g:netrw_banner = 0          # Hide top help banner
g:netrw_liststyle = 3       # Tree view style
g:netrw_winsize = 25        # Set drawer width to 25%

# ==============================================================================
# Command Line Abbreviations
# ==============================================================================
cnoreabbrev W! w!
cnoreabbrev Q! q!
cnoreabbrev Qall! qall!
cnoreabbrev Wq wq
cnoreabbrev Wa wa
cnoreabbrev wQ wq
cnoreabbrev WQ wq
cnoreabbrev W w
cnoreabbrev Q q
cnoreabbrev Qall qall

# ==============================================================================
# Autocmd Rules
# ==============================================================================
augroup Vim9Defaults
    autocmd!
    # Sync syntax highlighting
    autocmd BufEnter * syntax sync maxlines=200

    # Remember last cursor position when reopening files
    autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | execute "normal! g`\"" | endif

    # Text wrapping for .txt files
    autocmd BufRead,BufNewFile *.txt setlocal wrap wm=2 textwidth=79

    # Filetype adjustments
    autocmd FileType make setlocal noexpandtab
    autocmd BufNewFile,BufRead CMakeLists.txt setlocal filetype=cmake
    autocmd BufRead,BufNewFile *.swift setlocal filetype=swift
    autocmd BufRead,BufNewFile *.mdx setlocal filetype=markdown.mdx

    # 2-space indent for HTML and Shell scripts
    autocmd FileType html,sh setlocal ts=2 sw=2 expandtab

    # Fix Quickfix window height
    autocmd FileType qf :6wincmd _

    # Fix current working directory
    autocmd VimEnter * if isdirectory(expand('%:p')) | execute 'lcd ' .. expand('%:p') | endif
augroup END

# ==============================================================================
# Key Mappings
# ==============================================================================

# Fast escape out of insert mode
inoremap jj <Esc>

# Clear search highlights
nnoremap <silent> <leader><space> :noh<CR>

# Built-in Netrw File Explorer Toggle
nnoremap <leader>o :Lexplore<CR>

# Splits
noremap <Leader>h :<C-u>split<CR>
noremap <Leader>v :<C-u>vsplit<CR>

# Window Navigation
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
noremap <C-h> <C-w>h

# Keep Visual Selection after indenting
vmap < <gv
vmap > >gv

# Move selected block up/down
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

# Buffer Navigation
noremap <leader>z :bp<CR>
noremap <leader>q :bp<CR>
noremap <leader>x :bn<CR>
noremap <leader>w :bn<CR>
noremap <leader>bd :bp<bar>bd #<CR>

# Open/Close Quickfix window
nnoremap <leader>q :copen<CR>

# Jump back & forth through results with ]q and [q
nnoremap ]q :cnext<CR>
nnoremap [q :cprev<CR>

# Sudo Write (save read-only files)
cmap w!! w !sudo tee % >/dev/null

# Directory Helpers
nnoremap <leader>. :lcd %:p:h<CR>
noremap <Leader>e :e <C-R>=expand("%:p:h") . "/"<CR>
