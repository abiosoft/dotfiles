
" fugitive
set statusline+=%{fugitive#statusline()}

if has('nvim')
    set diffopt+=vertical
else
    set diffopt-=internal
    set diffopt+=vertical
endif

autocmd BufReadPost fugitive://* set bufhidden=delete

" use fzf for checkout
" Use `:Format` to format current buffer
command! -nargs=0 Gcheckout :!bash -c 'git checkout $(git branch -a | fzf --reverse --no-preview)' <CR>

